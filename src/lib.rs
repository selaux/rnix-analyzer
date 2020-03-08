pub use rnix;
pub use rnix::{TextRange, TextUnit, AST};

pub mod references;
pub mod scope;

use references::References;
pub use references::{Reference, ReferenceError, Variable};
use scope::Scopes;
pub use scope::{
    Definition, DefinitionId, InverseScopeTree, Scope, ScopeAnalysisError, ScopeId, ScopeKind,
};

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
    scopes: Scopes,
    references: References,
    errors: Vec<AnalysisError>,
}

impl AnalysisResult {
    /// Analyze the code within `ast` and return the resulting `AnalysisResult`
    pub fn from(ast: &AST) -> Self {
        let (scopes, scope_errors) = scope::collect_scopes(&ast);
        let (references, reference_errors) = References::from_ast_and_scope_tree(ast, &scopes);
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

    /// Returns the inverse scope tree
    pub fn inverse_scope_tree(&self) -> &InverseScopeTree {
        self.scopes.inverse_scope_tree()
    }
}
