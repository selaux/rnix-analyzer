use crate::Scope;
use rnix::TextRange;

/// InverseScopeTree refers to single instance of a path in the scope tree.
#[derive(Debug, PartialEq, Clone)]
pub struct ScopePath(Vec<Scope>);

/// InverseScopeTree refers to a tree of scopes, optimized for seaching Scopes that contain a specific text range.
#[derive(Debug, PartialEq, Clone)]
pub struct InverseScopeTree {
    /// This vec is ordered by scope size, so a simple find should find the most specific scope
    leaf_scopes: Vec<ScopePath>,
}

impl InverseScopeTree {
    /// Build a inverse scope tree from a list of scopes that might include each other
    pub fn from_scopes(scopes: &[Scope]) -> InverseScopeTree {
        let mut leaf_scopes = vec![];
        let mut scopes = scopes.to_owned();
        scopes.sort_by(|s1, s2| s1.text_range.len().cmp(&s2.text_range.len()));
        for scope in scopes.iter() {
            let mut scope_path = vec![scope.clone()];
            for other_scope in scopes.iter() {
                if scope != other_scope && scope.text_range.is_subrange(&other_scope.text_range) {
                    scope_path.push(other_scope.clone());
                }
            }
            leaf_scopes.push(ScopePath(scope_path));
        }

        InverseScopeTree { leaf_scopes }
    }

    pub fn get_scopes(&self, range: TextRange) -> Option<impl Iterator<Item = &Scope>> {
        let scope = self.leaf_scopes.iter().find(|scopes| {
            range.is_subrange(&scopes.0.first().expect("more than one node").text_range)
        })?;
        Some(scope.0.iter())
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;
    use crate::{AnalysisError, AnalysisOptions, AnalysisResult};
    use insta::assert_debug_snapshot;
    use rnix::TextUnit;
    use std::process::Command;
    use std::str;

    #[test]
    fn test_simple_get_scopes() {
        assert_from_scopes_snapshot(
            "a: 1",
            TextRange::from_to(TextUnit::from(0), TextUnit::from(1)),
        );
    }

    #[test]
    fn test_nested_get_scopes() {
        assert_from_scopes_snapshot(
            "a: b: { c = 1; }",
            TextRange::from_to(TextUnit::from(8), TextUnit::from(9)),
        );
    }

    fn assert_from_scopes_snapshot(nix_code: &str, range: TextRange) {
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
        let scopes: Vec<_> = scope_tree
            .get_scopes(range)
            .expect("should return a scope")
            .collect();
        assert_debug_snapshot!(scopes);
    }
}
