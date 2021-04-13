use crate::{utils::Stack, CollectFromTree, DefinitionId, Scope, ScopeKind};
use id_arena::{Arena, Id as ArenaId};
use rnix::types::{BinOp, BinOpKind, Ident, Inherit, Select, TokenWrapper, TypedNode};
use rnix::{SyntaxKind, SyntaxNode, TextRange};
use std::collections::BTreeMap;

pub type VariableId = ArenaId<Variable>;

/// A variable occurence in code
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Variable {
    /// Unique id of the variable
    pub id: VariableId,
    /// defined name
    pub name: String,
    /// location of the variable
    pub text_range: TextRange,
}

/// An error that occured during reference analysis
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ReferenceError {
    NotFoundInScope(String, TextRange),
}

/// A reference from a variable occurence to a definition
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Reference {
    pub from: VariableId,
    pub to: DefinitionId,
}

/// The result of reference analysis
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct References {
    variable_arena: Arena<Variable>,
    references: BTreeMap<VariableId, Reference>,
}

fn filter_identifier(
    parents: &Stack<SyntaxNode>,
    in_scopes: &Stack<Scope>,
    in_select: &Stack<Select>,
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
    {
        return !in_scopes
            .iter()
            .find(|v| v.kind() == ScopeKind::AttrSet || v.kind() == ScopeKind::RecursiveAttrSet)
            .map(|v| v.kind() == ScopeKind::RecursiveAttrSet)
            .unwrap_or(false);
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
    // Filter identifiers in attrset contains binary operator
    if last_four_kinds[0] == Some(SyntaxKind::NODE_BIN_OP) || in_select.get(0).is_some() {
        let binop = parents
            .iter()
            .skip_while(|node| node.kind() == SyntaxKind::NODE_SELECT)
            .find(|node| node.kind() == SyntaxKind::NODE_BIN_OP)
            .cloned()
            .and_then(BinOp::cast);
        if let Some(binop) = binop {
            let kind = binop.operator();
            let im_right = binop
                .rhs()
                .map(|n| node.text_range().is_subrange(&n.text_range()))
                .unwrap_or(false);
            if kind == BinOpKind::IsSet && im_right {
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
    /// Returns a variable by id
    pub fn variable(&self, id: &VariableId) -> Option<&Variable> {
        self.variable_arena.get(*id)
    }
    /// Returns all variables
    pub fn variables(&self) -> impl Iterator<Item = &Variable> {
        self.variable_arena.iter().map(|(_id, val)| val)
    }

    /// Returns all variables within a range
    pub fn variables_at(&self, range: TextRange) -> impl Iterator<Item = &Variable> {
        self.variable_arena
            .iter()
            .map(|(_id, variable)| variable)
            .filter(move |variable| variable.text_range.intersection(&range).is_some())
    }

    /// Get all variables for a definition
    pub fn variables_for(&self, definition_id: &DefinitionId) -> impl Iterator<Item = &Variable> {
        let definition_id = *definition_id;
        let variable_arena = &self.variable_arena;
        self.references
            .iter()
            .filter(move |(_id, reference)| reference.to == definition_id)
            .map(move |(_id, reference)| &variable_arena[reference.from])
    }

    /// Definition of variable
    pub fn definition_of(&self, variable: &Variable) -> Option<&DefinitionId> {
        self.references.get(&variable.id).map(|v| &v.to)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct TrackReferencesDependencies<'a, 'b> {
    parents: &'a Stack<SyntaxNode>,
    current_scopes: &'b Stack<Scope>,
}

impl TrackReferencesDependencies<'_, '_> {
    pub fn new<'a, 'b>(
        parents: &'a Stack<SyntaxNode>,
        current_scopes: &'b Stack<Scope>,
    ) -> TrackReferencesDependencies<'a, 'b> {
        TrackReferencesDependencies {
            parents,
            current_scopes,
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct TrackReferencesState {
    pub(crate) variable_arena: Arena<Variable>,
    pub(crate) references: BTreeMap<VariableId, Reference>,
    pub(crate) errors: Vec<ReferenceError>,
    pub(crate) in_select: Stack<Select>,
}

#[derive(Clone, Default)]
pub(crate) struct TrackReferences {
    state: TrackReferencesState,
}

impl TrackReferences {
    pub fn new() -> Self {
        Default::default()
    }
}

impl<'a, 'b> CollectFromTree<TrackReferencesDependencies<'a, 'b>> for TrackReferences {
    type State = TrackReferencesState;
    type Result = References;
    type Error = ReferenceError;

    fn enter_node(
        &mut self,
        dependencies: TrackReferencesDependencies<'a, 'b>,
        node: &rnix::SyntaxNode,
    ) {
        let parents = dependencies.parents;
        let in_scopes = dependencies.current_scopes;
        if node.kind() == SyntaxKind::NODE_SELECT {
            let select = Select::cast(node.clone()).expect("select should be castable");
            self.state.in_select.push_front(select);
        }
        if node.kind() == SyntaxKind::NODE_IDENT
            && !filter_identifier(parents, in_scopes, &self.state.in_select, node)
        {
            if let Some(ident) = Ident::cast(node.clone()) {
                let name = ident.as_str().to_owned();
                let id = self.state.variable_arena.alloc_with_id(|id| Variable {
                    id,
                    name: name.clone(),
                    text_range: ident.node().text_range(),
                });
                let definition = in_scopes
                    .iter()
                    .filter_map(|scope| scope.definition(&name))
                    .next();
                if let Some(definition) = definition {
                    self.state.references.insert(
                        id,
                        Reference {
                            from: id,
                            to: *definition,
                        },
                    );
                } else {
                    let non_exhaustive = in_scopes.iter().any(|scope| !scope.is_exhaustive());
                    if !non_exhaustive {
                        self.state.errors.push(ReferenceError::NotFoundInScope(
                            name,
                            ident.node().text_range(),
                        ));
                    }
                }
            }
        }
    }

    fn exit_node(&mut self, _: TrackReferencesDependencies<'a, 'b>, node: &rnix::SyntaxNode) {
        if node.kind() == SyntaxKind::NODE_SELECT {
            self.state.in_select.pop_front();
        }
    }

    fn state(&self) -> &Self::State {
        &self.state
    }

    fn result(self) -> (Self::Result, Vec<Self::Error>) {
        (
            References {
                variable_arena: self.state.variable_arena,
                references: self.state.references,
            },
            self.state.errors,
        )
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use crate::{AnalysisError, AnalysisResult};
    use insta::assert_display_snapshot;
    use std::process::Command;
    use std::str;

    #[test]
    fn test_references_simple_function() {
        assert_refences_snapshot("a: a");
    }

    #[test]
    fn test_references_simple_function_with_binop() {
        assert_refences_snapshot("a: a + a");
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
    fn test_references_object_contains_deep_path() {
        assert_refences_snapshot("let a.b = 1; in a ? c.d.e");
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
        let mut variables: Vec<_> = result.references.variables().collect();
        variables.sort_by(|a, b| a.id.cmp(&b.id));
        assert_display_snapshot!(format!(
            "{}\n=========\n{:#?}\n=========\n{:#?}",
            nix_code, variables, result.references.references
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
        let errors: Vec<AnalysisError> = result.errors().cloned().collect();
        assert_display_snapshot!(format!("{}\n=========\n{:#?}", nix_code, errors));
    }
}
