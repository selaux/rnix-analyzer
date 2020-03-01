use crate::{Definition, InverseScopeTree};
use rnix::types::{
    AttrSet, BinOp, BinOpKind, Ident, Inherit, Key, KeyValue, TokenWrapper, TypedNode,
};
use rnix::{SyntaxKind, TextRange, AST};
use std::cmp::Ordering;
use std::collections::BTreeMap;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Identifier {
    pub name: String,
    pub text_range: TextRange,
}

impl Ord for Identifier {
    fn cmp(&self, other: &Self) -> Ordering {
        self.text_range.start().cmp(&other.text_range.start())
    }
}

impl PartialOrd for Identifier {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.text_range.start().cmp(&other.text_range.start()))
    }
}

fn collect_identifiers(ast: &AST) -> impl Iterator<Item = Identifier> {
    ast.node()
        .descendants()
        .filter_map(Ident::cast)
        .filter(|ident| {
            // Filter nested identifiers
            let previous_kind = ident.node().prev_sibling_or_token().map(|prev| prev.kind());
            previous_kind == None || previous_kind != Some(SyntaxKind::TOKEN_DOT)
        })
        .filter(|ident| {
            // Filter identifiers in non recursive objects
            let parent = ident
                .node()
                .parent()
                .and_then(Key::cast)
                .and_then(|parent| parent.node().parent())
                .and_then(KeyValue::cast)
                .and_then(|parent| parent.node().parent())
                .and_then(AttrSet::cast);
            if let Some(attrset) = parent {
                attrset.recursive()
            } else {
                true
            }
        })
        .filter(|ident| {
            // Filter identifiers that are inherit from
            let parent = ident
                .node()
                .parent()
                .and_then(Inherit::cast)
                .and_then(|inherit| inherit.from());
            parent.is_none()
        })
        .filter(|ident| {
            // Filter identifiers in object contains
            let binop = ident
                .node()
                .parent()
                .and_then(|node| match node.kind() {
                    SyntaxKind::NODE_BIN_OP => Some(node),
                    SyntaxKind::NODE_SELECT => node.parent(),
                    _ => None,
                })
                .and_then(BinOp::cast);
            if let Some(binop) = binop {
                let im_right = binop
                    .rhs()
                    .map(|node| node.text_range().start() == ident.node().text_range().start())
                    .unwrap_or(false);
                let kind = binop.operator();
                kind != BinOpKind::IsSet || !im_right
            } else {
                true
            }
        })
        .map(|ident| Identifier {
            name: ident.as_str().to_owned(),
            text_range: ident.node().text_range(),
        })
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ReferenceError {
    NotFoundInScope(Identifier),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Reference {
    from: Identifier,
    to: Definition,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct References {
    identifiers: Vec<Identifier>,
    references: BTreeMap<Identifier, Reference>,
}

impl References {
    pub fn from_ast_and_scope_tree(
        ast: &AST,
        scope_tree: &InverseScopeTree,
    ) -> (Self, Vec<ReferenceError>) {
        let identifiers: Vec<_> = collect_identifiers(ast).collect();
        let mut errors = vec![];
        let mut references = BTreeMap::new();

        for identifier in identifiers.iter() {
            let scopes: Vec<_> = scope_tree
                .get_scopes(identifier.text_range)
                .into_iter()
                .flatten()
                .collect();
            let definition = scopes
                .iter()
                .filter_map(|scope| scope.get_definition(&identifier.name))
                .next();
            let non_exhaustive = scopes.iter().any(|scope| !scope.is_exhaustive());

            if let Some(definition) = definition {
                references.insert(
                    identifier.clone(),
                    Reference {
                        from: identifier.clone(),
                        to: definition,
                    },
                );
            } else if !non_exhaustive {
                errors.push(ReferenceError::NotFoundInScope(identifier.clone()));
            }
        }

        (
            References {
                identifiers,
                references,
            },
            errors,
        )
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;
    use crate::{AnalysisError, AnalysisOptions, AnalysisResult};
    use insta::assert_debug_snapshot;
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
        let analysis_options = AnalysisOptions {};
        let result = AnalysisResult::from(&parse_result, &analysis_options);
        let errors: Vec<AnalysisError> = result.errors().cloned().collect();
        assert_eq!(errors, vec![]);
        let scopes: Vec<_> = result.scopes().cloned().collect();
        let scope_tree = InverseScopeTree::from_scopes(scopes.as_slice());
        let references = References::from_ast_and_scope_tree(&parse_result, &scope_tree);
        assert_eq!(references.1, vec![]);
        assert_debug_snapshot!(references.0);
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
        let analysis_options = AnalysisOptions {};
        let result = AnalysisResult::from(&parse_result, &analysis_options);
        let scopes: Vec<_> = result.scopes().cloned().collect();
        let scope_tree = InverseScopeTree::from_scopes(scopes.as_slice());
        let references = References::from_ast_and_scope_tree(&parse_result, &scope_tree);
        assert_debug_snapshot!(references.1);
    }
}
