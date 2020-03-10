use std::collections::VecDeque;

pub use rnix;
use rnix::{SyntaxNode, WalkEvent};
pub use rnix::{TextRange, TextUnit, AST};

pub mod references;
pub mod scope;

use references::References;
pub use references::{Reference, ReferenceError, Variable, VariableId};
use scope::Scopes;
pub use scope::{
    Definition, DefinitionId, InverseScopeTree, Scope, ScopeAnalysisError, ScopeId, ScopeKind,
};

pub(crate) trait CollectFromTree<D> {
    type State;
    type Result;
    type Error;

    fn enter_node(&mut self, dependencies: D, node: &rnix::SyntaxNode);

    fn exit_node(&mut self, dependencies: D, node: &rnix::SyntaxNode);

    fn state(&self) -> &Self::State;

    fn result(self) -> (Self::Result, Vec<Self::Error>);
}

#[derive(Debug, PartialEq, Clone, Default)]
pub(crate) struct TrackParent {
    pub(crate) state: VecDeque<SyntaxNode>,
}

impl TrackParent {
    pub fn new() -> Self {
        Default::default()
    }
}

impl CollectFromTree<()> for TrackParent {
    type State = VecDeque<SyntaxNode>;
    type Result = ();
    type Error = ();

    fn enter_node(&mut self, _: (), node: &rnix::SyntaxNode) {
        self.state.push_front(node.clone());
    }

    fn exit_node(&mut self, _: (), _: &rnix::SyntaxNode) {
        self.state.pop_front();
    }

    fn state(&self) -> &Self::State {
        &self.state
    }

    fn result(self) -> (Self::Result, Vec<Self::Error>) {
        ((), vec![])
    }
}

/// Error that occured during code analysis
#[derive(Debug, PartialEq, Clone)]
pub enum AnalysisError {
    /// Any error that ocurred during reference analysis
    Reference(ReferenceError),
    /// Any error that occured during scope analysis
    Scope(ScopeAnalysisError),
}

impl From<&ScopeAnalysisError> for AnalysisError {
    fn from(other: &ScopeAnalysisError) -> Self {
        AnalysisError::Scope(other.clone())
    }
}

impl From<&ReferenceError> for AnalysisError {
    fn from(other: &ReferenceError) -> Self {
        AnalysisError::Reference(other.clone())
    }
}

/// Analysis result of a code analysis run
#[derive(Debug, PartialEq, Clone)]
pub struct AnalysisResult {
    pub(crate) scopes: Scopes,
    pub(crate) references: References,
    errors: Vec<AnalysisError>,
}

impl AnalysisResult {
    /// Analyze the code within `ast` and return the resulting `AnalysisResult`
    pub fn from(ast: &AST) -> Self {
        let mut track_parents = TrackParent::new();
        let mut scopes = scope::TrackScopes::new(ast);
        let mut references = references::TrackReferences::new();
        for walk_event in ast.node().preorder() {
            match walk_event {
                WalkEvent::Enter(node) => {
                    track_parents.enter_node((), &node);
                    scopes.enter_node((), &node);
                    let parents = track_parents.state();
                    let scope_state = scopes.state();
                    let deps = references::TrackReferencesDependencies::new(
                        &parents,
                        &scope_state.current_scopes,
                    );
                    references.enter_node(deps, &node);
                }
                WalkEvent::Leave(node) => {
                    let parents = track_parents.state();
                    let scope_state = scopes.state();
                    let deps = references::TrackReferencesDependencies::new(
                        &parents,
                        &scope_state.current_scopes,
                    );
                    references.exit_node(deps, &node);
                    scopes.exit_node((), &node);
                    track_parents.exit_node((), &node);
                }
            }
        }
        let (scopes, scope_errors) = scopes.result();
        let (references, reference_errors) = references.result();
        let errors: Vec<_> = scope_errors
            .iter()
            .map(AnalysisError::from)
            .chain(reference_errors.iter().map(AnalysisError::from))
            .collect();
        AnalysisResult {
            scopes,
            references,
            errors,
        }
    }

    /// Returns errors that occurred during code analysis
    pub fn errors(&self) -> impl Iterator<Item = &AnalysisError> {
        self.errors.iter()
    }

    /// Returns a definition by id
    pub fn definition(&self, definition_id: &DefinitionId) -> Option<&Definition> {
        self.scopes.definition(definition_id)
    }

    /// Returns all definitions of variables encountered in the code
    ///
    /// ```rust
    /// use rnix_analyzer::*;
    ///
    /// let ast = rnix::parse("let foo = 1; in a");
    /// let analysis = AnalysisResult::from(&ast);
    ///
    /// // This is a definition in a let binding
    /// assert!(analysis.definitions().find(|d| d.name == "foo").is_some());
    /// // This is a definition that is defined by nix itself
    /// assert!(analysis.definitions().find(|d| d.name == "builtins").is_some());
    /// // This does not exist
    /// assert!(analysis.definitions().find(|d| d.name == "bar").is_none());
    /// ```
    pub fn definitions(&self) -> impl Iterator<Item = &Definition> {
        self.scopes.definitions()
    }

    /// Returns definition for a variable encountered in the code
    ///
    /// ```rust
    /// use rnix_analyzer::*;
    ///
    /// let ast = rnix::parse("let foo = 1; in foo");
    /// let analysis = AnalysisResult::from(&ast);
    /// let foo = analysis.variables_at(TextRange::from_to(
    ///     TextUnit::from(16),
    ///     TextUnit::from(17)
    /// )).next().unwrap();
    ///
    /// assert_eq!(analysis.definition_of(&foo).unwrap().text_range, TextRange::from_to(
    ///     TextUnit::from(4),
    ///     TextUnit::from(7)
    /// ));
    /// ```
    pub fn definition_of(&self, variable: &Variable) -> Option<&Definition> {
        self.references
            .definition_of(variable)
            .and_then(|id| self.scopes.definition(id))
    }

    /// Returns a scope by id
    pub fn scope(&self, scope_id: &ScopeId) -> Option<&Scope> {
        self.scopes.scope(scope_id)
    }

    /// Returns all scopes encountered in the code, including the root scope
    ///
    /// ```rust
    /// use rnix_analyzer::*;
    ///
    /// let ast = rnix::parse("a: a");
    /// let analysis = AnalysisResult::from(&ast);
    ///
    /// assert_eq!(analysis.scopes().count(), 2);
    /// ```
    pub fn scopes(&self) -> impl Iterator<Item = &Scope> {
        self.scopes.scopes()
    }

    /// Returns scopes that fully contain the passed range
    /// ```rust
    /// use rnix_analyzer::*;
    ///
    /// let ast = rnix::parse("a: b: c");
    /// let analysis = AnalysisResult::from(&ast);
    ///
    /// // Scopes for `a:`
    /// assert_eq!(analysis.scopes_at(TextRange::from_to(
    ///     TextUnit::from(0),
    ///     TextUnit::from(1)
    /// )).count(), 2);
    /// // Scopes for `b:`
    /// assert_eq!(analysis.scopes_at(TextRange::from_to(
    ///     TextUnit::from(3),
    ///     TextUnit::from(4)
    /// )).count(), 3);
    /// ```
    pub fn scopes_at(&self, range: TextRange) -> impl Iterator<Item = &Scope> {
        self.scopes.scopes_at(range)
    }

    /// Returns a variable by id
    pub fn variable(&self, variable_id: &VariableId) -> Option<&Variable> {
        self.references.variable(variable_id)
    }

    /// Returns all variables encountered in the code, including places where they are defined
    ///
    /// ```rust
    /// use rnix_analyzer::*;
    ///
    /// let ast = rnix::parse("a: b: c");
    /// let analysis = AnalysisResult::from(&ast);
    /// let variables: Vec<_> = analysis.variables().map(|v| v.name.as_str()).collect();
    ///
    /// assert_eq!(variables, vec!["a", "b", "c"]);
    /// ```
    pub fn variables(&self) -> impl Iterator<Item = &Variable> {
        self.references.variables()
    }

    /// Returns all variables within a text range
    ///
    /// ```rust
    /// use rnix_analyzer::*;
    ///
    /// let ast = rnix::parse("a: b: c");
    /// let analysis = AnalysisResult::from(&ast);
    /// let variables: Vec<_> = analysis.variables_at(TextRange::from_to(TextUnit::from(3), TextUnit::from(7))).map(|v| v.name.as_str()).collect();
    ///
    /// assert_eq!(variables, vec!["b", "c"]);
    /// ```
    pub fn variables_at(&self, range: TextRange) -> impl Iterator<Item = &Variable> {
        self.references.variables_at(range)
    }

    /// Returns all variables for a definition
    ///
    /// ```rust
    /// use rnix_analyzer::*;
    ///
    /// let ast = rnix::parse("let foo = 1; in foo + foo");
    /// let analysis = AnalysisResult::from(&ast);
    /// let foo = analysis.variables_at(TextRange::from_to(
    ///     TextUnit::from(16),
    ///     TextUnit::from(17)
    /// )).next().unwrap();
    /// let foo_def = analysis.definition_of(&foo).unwrap();
    /// let foo_occurences: Vec<_> = analysis.variables_for(&foo_def).map(|v| (v.name.clone(), v.text_range)).collect();
    ///
    /// assert_eq!(foo_occurences, vec![
    ///     ("foo".to_string(), TextRange::from_to(
    ///         TextUnit::from(4),
    ///         TextUnit::from(7)
    ///     )),
    ///     ("foo".to_string(), TextRange::from_to(
    ///         TextUnit::from(16),
    ///         TextUnit::from(19)
    ///     )),
    ///     ("foo".to_string(), TextRange::from_to(
    ///         TextUnit::from(22),
    ///         TextUnit::from(25)
    ///     )),
    /// ]);
    /// ```
    pub fn variables_for(&self, definition: &Definition) -> impl Iterator<Item = &Variable> {
        self.references.variables_for(&definition.id)
    }

    /// Returns the inverse scope tree
    pub fn inverse_scope_tree(&self) -> &InverseScopeTree {
        self.scopes.inverse_scope_tree()
    }
}

#[cfg(test)]
mod tests {
    use rnix::{TextRange, TextUnit};
    #[test]
    fn test_empty_text_range_should_be_a_subrange_and_intersect() {
        let containing = TextRange::from_to(TextUnit::from(0), TextUnit::from(10));
        let inside = TextRange::from_to(TextUnit::from(1), TextUnit::from(1));

        // This basically tests assumptions
        assert!(inside.is_subrange(&containing));
        assert!(inside.intersection(&containing).is_some());
    }
}
