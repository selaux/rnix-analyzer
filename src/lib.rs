use rnix::{TextRange, AST};

pub mod references;
pub mod scope;

pub use references::{Reference, ReferenceError, References, Variable};
pub use scope::{
    Definition, DefinitionId, InverseScopeTree, Scope, ScopeAnalysisError, ScopeId, ScopeKind,
    Scopes,
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
    /// let ast = rnix::parse("let foo = 1; in a");
    /// let analysis = rnix_analyzer::AnalysisResult::from(&ast);
    ///
    /// assert!(analysis.definitions().find(|d| d.name == "foo").is_some());
    /// assert!(analysis.definitions().find(|d| d.name == "builtins").is_some());
    /// assert!(analysis.definitions().find(|d| d.name == "bar").is_none());
    /// ```
    pub fn definitions(&self) -> impl Iterator<Item = &Definition> {
        self.scopes.definitions()
    }

    /// Returns all scopes encountered in the code, including the root scope
    ///
    /// ```rust
    /// let ast = rnix::parse("a: a");
    /// let analysis = rnix_analyzer::AnalysisResult::from(&ast);
    ///
    /// assert_eq!(analysis.scopes().count(), 2);
    /// ```
    pub fn scopes(&self) -> impl Iterator<Item = &Scope> {
        self.scopes.scopes()
    }

    /// Returns the applicable scopes for a given text range
    pub fn scopes_at(&self, range: TextRange) -> impl Iterator<Item = &Scope> {
        self.scopes.scopes_at(range)
    }

    /// Returns all variables encountered in the code
    pub fn variables(&self) -> impl Iterator<Item = &Variable> {
        self.references.variables()
    }

    /// Returns the inverse scope tree
    pub fn inverse_scope_tree(&self) -> &InverseScopeTree {
        self.scopes.inverse_scope_tree()
    }
}
