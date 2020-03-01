use rnix::AST;

mod scope;

/// Options used for code analysis
#[derive(Debug, PartialEq, Clone)]
pub struct AnalysisOptions {}

pub use scope::{Definition, Scope, ScopeAnalysisError, ScopeKind};

/// Error that occured during code analysis
#[derive(Debug, PartialEq, Clone)]
pub enum AnalysisError {
    /// Any error that occured during scope analysis
    Scope(scope::ScopeAnalysisError),
}

impl From<&ScopeAnalysisError> for AnalysisError {
    fn from(other: &ScopeAnalysisError) -> Self {
        AnalysisError::Scope(other.clone())
    }
}

/// Analysis result of a code analysis run
#[derive(Debug, PartialEq, Clone)]
pub struct AnalysisResult {
    scopes: Vec<scope::Scope>,
    scope_errors: Vec<AnalysisError>,
}

impl AnalysisResult {
    /// Analyze the code within `ast` and return the resulting `AnalysisResult`
    pub fn from(ast: &AST, _options: &AnalysisOptions) -> Self {
        let scope_analysis_result = scope::collect_scopes(&ast);
        let scope_errors: Vec<_> = scope_analysis_result
            .1
            .iter()
            .map(AnalysisError::from)
            .collect();
        AnalysisResult {
            scopes: scope_analysis_result.0,
            scope_errors,
        }
    }

    /// Returns errors that occurred during code analysis
    pub fn errors(&self) -> impl Iterator<Item = &AnalysisError> {
        self.scope_errors.iter()
    }

    /// Returns all scopes encountered in the code
    pub fn scopes(&self) -> impl Iterator<Item = &scope::Scope> {
        self.scopes.iter()
    }
}
