use crate::{scope::defines_scope, DefinitionId, Scope, ScopeKind, Scopes};
use id_arena::{Arena, Id as ArenaId};
use rnix::types::{AttrSet, BinOp, Ident, Inherit, TokenWrapper, TypedNode};
use rnix::{SyntaxKind, SyntaxNode, TextRange, WalkEvent, AST};
use std::collections::{BTreeMap, VecDeque};

type IdentifierId = ArenaId<Identifier>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Identifier {
    pub id: IdentifierId,
    pub name: String,
    pub text_range: TextRange,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ReferenceError {
    NotFoundInScope(String, TextRange),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Reference {
    from: IdentifierId,
    to: DefinitionId,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct References {
    identifier_arena: Arena<Identifier>,
    pub references: BTreeMap<IdentifierId, Reference>,
}

fn filter_identifier(
    parents: &VecDeque<SyntaxNode>,
    current_attrset_recursive: &VecDeque<bool>,
    node: &SyntaxNode,
) -> bool {
    let last_four_parents: Vec<_> = (1..4).map(|v| parents.get(v)).collect();
    let last_four_kinds: Vec<_> = last_four_parents
        .iter()
        .map(|n| n.map(|n| n.kind()))
        .collect();

    // Filter identifiers in select paths (except first one)
    if (last_four_kinds[0] == Some(SyntaxKind::NODE_SELECT)
        || last_four_kinds[0] == Some(SyntaxKind::NODE_KEY))
        && node.prev_sibling_or_token().map(|prev| prev.kind()) == Some(SyntaxKind::TOKEN_DOT)
    {
        return true;
    }
    // Filter identifiers in normal attrset paths
    if last_four_kinds[0] == Some(SyntaxKind::NODE_KEY)
        && last_four_kinds[1] == Some(SyntaxKind::NODE_KEY_VALUE)
        && last_four_kinds[2] == Some(SyntaxKind::NODE_ATTR_SET)
        && !current_attrset_recursive[0]
    {
        return true;
    }
    // Filter identifiers in from clause of inherit
    if last_four_kinds[0] == Some(SyntaxKind::NODE_INHERIT) {
        let parent = last_four_parents[0]
            .cloned()
            .and_then(Inherit::cast)
            .and_then(|inherit| inherit.from());
        if parent.is_some() {
            return true;
        }
    }
    // Filter identifiers in object contains binary operator
    if last_four_kinds[0] == Some(SyntaxKind::NODE_BIN_OP) {
        let parent = last_four_parents[0].cloned().and_then(BinOp::cast);
        if let Some(parent) = parent {
            let im_right = parent
                .rhs()
                .map(|n| node.text_range().is_subrange(&n.text_range()))
                .unwrap_or(false);
            if im_right {
                return true;
            }
        }
    }
    if last_four_kinds[0] == Some(SyntaxKind::NODE_SELECT)
        && last_four_kinds[1] == Some(SyntaxKind::NODE_BIN_OP)
    {
        let parent = last_four_parents[1].cloned().and_then(BinOp::cast);
        if let Some(parent) = parent {
            let im_right = parent
                .rhs()
                .map(|n| node.text_range().is_subrange(&n.text_range()))
                .unwrap_or(false);
            if im_right {
                return true;
            }
        }
    }
    false
}

impl References {
    pub fn from_ast_and_scope_tree(ast: &AST, scopes: &Scopes) -> (Self, Vec<ReferenceError>) {
        let mut errors = vec![];
        let mut references = BTreeMap::new();
        let mut identifier_arena = Arena::new();
        let mut current_attrset_recursive: VecDeque<bool> = VecDeque::new();
        let mut parents: VecDeque<SyntaxNode> = VecDeque::new();
        let mut in_scopes: VecDeque<&Scope> = VecDeque::new();

        in_scopes.push_front(scopes.root_scope());

        for walk_event in ast.node().preorder() {
            match walk_event {
                WalkEvent::Enter(node) => {
                    let kind = node.kind();
                    parents.push_front(node.clone());
                    if defines_scope(kind) {
                        let scope = scopes
                            .scope_arena
                            .iter()
                            .map(|v| v.1)
                            .find(|scope| {
                                scope.text_range == node.text_range()
                                    && scope.kind != ScopeKind::Root
                            })
                            .expect("scope should exist");
                        in_scopes.push_front(scope);
                    }

                    match kind {
                        SyntaxKind::NODE_ATTR_SET => {
                            let recursive = AttrSet::cast(node)
                                .map(|attrset| attrset.recursive())
                                .unwrap_or(false);
                            current_attrset_recursive.push_front(recursive);
                        }
                        SyntaxKind::NODE_IDENT => {
                            if !filter_identifier(&parents, &current_attrset_recursive, &node) {
                                if let Some(ident) = Ident::cast(node) {
                                    let name = ident.as_str().to_owned();
                                    let id = identifier_arena.alloc_with_id(|id| Identifier {
                                        id,
                                        name: name.clone(),
                                        text_range: ident.node().text_range(),
                                    });
                                    let definition = in_scopes
                                        .iter()
                                        .filter_map(|scope| scope.get_definition(&name))
                                        .next();
                                    if let Some(definition) = definition {
                                        references.insert(
                                            id,
                                            Reference {
                                                from: id,
                                                to: definition,
                                            },
                                        );
                                    } else {
                                        let non_exhaustive =
                                            in_scopes.iter().any(|scope| !scope.is_exhaustive());
                                        if !non_exhaustive {
                                            errors.push(ReferenceError::NotFoundInScope(
                                                name,
                                                ident.node().text_range(),
                                            ));
                                        }
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
                WalkEvent::Leave(node) => {
                    let kind = node.kind();
                    if kind == SyntaxKind::NODE_ATTR_SET {
                        current_attrset_recursive.pop_front();
                    }
                    if defines_scope(kind) {
                        in_scopes.pop_front();
                    }
                    parents.pop_front();
                }
            }
        }

        (
            References {
                identifier_arena,
                references,
            },
            errors,
        )
    }

    /// Returns the applicable scopes for a given text range
    pub fn get_identifiers(&self) -> Vec<&Identifier> {
        self.identifier_arena.iter().map(|(_id, val)| val).collect()
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;
    use crate::{AnalysisError, AnalysisResult};
    use insta::{assert_debug_snapshot, assert_display_snapshot};
    use std::process::Command;
    use std::str;

    #[test]
    fn test_references_simple_function() {
        assert_refences_snapshot("a: a");
    }

    #[test]
    fn test_references_function_pattern() {
        assert_refences_snapshot("{a,...}: a");
    }

    #[test]
    fn test_references_let_in() {
        assert_refences_snapshot("let a = 1; in a");
    }

    #[test]
    fn test_references_let_in_object() {
        assert_refences_snapshot("let a.b = 1; in a.b");
    }

    #[test]
    fn test_references_object_contains() {
        assert_refences_snapshot("let a.b = 1; in a ? c");
    }

    #[test]
    fn test_references_object_contains_path() {
        assert_refences_snapshot("let a.b = 1; in a ? c.d");
    }

    #[test]
    fn test_references_inherit_from() {
        assert_refences_snapshot("let a.b = 1; in { inherit (a) b; }");
    }

    #[test]
    fn test_references_object() {
        assert_refences_snapshot(
            "b: {
            a = 1;
            \"${b}\" = 2;
            c.d = 3;
        }",
        );
    }

    #[test]
    fn test_references_rec_object() {
        assert_refences_snapshot(
            "b: rec {
            a = 1;
            \"${b}\" = 2;
            c.d = a;
        }",
        );
    }

    #[test]
    fn test_error_undefined_reference_simple_function() {
        assert_refences_error_snapshot("a: b");
    }

    #[test]
    fn test_undefined_reference_inside_with() {
        assert_refences_snapshot("a: with a; b");
    }

    fn assert_refences_snapshot(nix_code: &str) {
        let output = Command::new("nix")
            .args(&["eval", &format!("({})", nix_code)])
            .output()
            .expect("failed to run nix");
        println!("{}", str::from_utf8(&output.stdout).unwrap());
        eprintln!("{}", str::from_utf8(&output.stderr).unwrap());
        assert_eq!(output.status.code(), Some(0));
        let parse_result = rnix::parse(nix_code);
        assert_eq!(parse_result.errors(), vec![]);
        let result = AnalysisResult::from(&parse_result);
        let errors: Vec<AnalysisError> = result.errors().cloned().collect();
        assert_eq!(errors, vec![]);
        let references = References::from_ast_and_scope_tree(&parse_result, &result.scopes);
        assert_eq!(references.1, vec![]);
        assert_display_snapshot!(format!(
            "{}\n=========\n{:#?}\n=========\n{:#?}",
            nix_code,
            references.0.get_identifiers(),
            references.0.references
        ));
    }

    fn assert_refences_error_snapshot(nix_code: &str) {
        let output = Command::new("nix")
            .args(&["eval", &format!("({})", nix_code)])
            .output()
            .expect("failed to run nix");
        println!("{}", str::from_utf8(&output.stdout).unwrap());
        eprintln!("{}", str::from_utf8(&output.stderr).unwrap());
        assert_eq!(output.status.code(), Some(1));
        let parse_result = rnix::parse(nix_code);
        assert_eq!(parse_result.errors(), vec![]);
        let result = AnalysisResult::from(&parse_result);
        let references = References::from_ast_and_scope_tree(&parse_result, &result.scopes);
        assert_debug_snapshot!(references.1);
    }
}
