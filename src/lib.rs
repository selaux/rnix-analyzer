use rnix::AST;

mod references;
mod scope;

/// Options used for code analysis
#[derive(Debug, PartialEq, Clone)]
pub struct AnalysisOptions {}

pub use references::{Identifier, Reference, ReferenceError, References};
pub use scope::{Definition, InverseScopeTree, Scope, ScopeAnalysisError, ScopeKind};

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
    scopes: Vec<Scope>,
    scope_tree: InverseScopeTree,
    references: References,
    errors: Vec<AnalysisError>,
}

impl AnalysisResult {
    /// Analyze the code within `ast` and return the resulting `AnalysisResult`
    pub fn from(ast: &AST, _options: &AnalysisOptions) -> Self {
        let (scopes, scope_errors) = scope::collect_scopes(&ast);
        let scope_tree = InverseScopeTree::from_scopes(&scopes);
        let (references, reference_errors) = References::from_ast_and_scope_tree(ast, &scope_tree);
        let errors: Vec<_> = scope_errors
            .iter()
            .map(AnalysisError::from)
            .chain(reference_errors.iter().map(AnalysisError::from))
            .collect();
        AnalysisResult {
            scopes,
            scope_tree,
            references,
            errors,
        }
    }

    /// Returns errors that occurred during code analysis
    pub fn errors(&self) -> impl Iterator<Item = &AnalysisError> {
        self.errors.iter()
    }

    /// Returns all scopes encountered in the code
    pub fn scopes(&self) -> impl Iterator<Item = &Scope> {
        self.scopes.iter()
    }
}
