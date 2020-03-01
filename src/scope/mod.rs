use rnix::{
    types::{
        AttrSet, EntryHolder, Ident, Lambda, LetIn, ParsedType, TokenWrapper, TypedNode, With,
    },
    SyntaxNode, TextRange, TextUnit,
};
use std::collections::BTreeMap;
use std::convert::TryFrom;

pub mod tree;
pub use tree::*;

/// A definition of a variable inside a Scope
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Definition {
    id: usize,
    name: String,
    text_range: TextRange,
}

fn insert_root_definition(
    idx: usize,
    name: &str,
    defines: &mut BTreeMap<String, Definition>,
) -> usize {
    defines.insert(
        name.to_owned(),
        Definition {
            id: idx,
            name: name.to_owned(),
            text_range: TextRange::from_to(TextUnit::from(0), TextUnit::from(0)),
        },
    );
    idx + 1
}

// Returns the defines of the root node
pub fn root_defines(mut idx: usize) -> (usize, BTreeMap<String, Definition>) {
    let mut defines = BTreeMap::new();

    idx = insert_root_definition(idx, "true", &mut defines);
    idx = insert_root_definition(idx, "false", &mut defines);
    idx = insert_root_definition(idx, "null", &mut defines);
    idx = insert_root_definition(idx, "throw", &mut defines);
    idx = insert_root_definition(idx, "abort", &mut defines);
    idx = insert_root_definition(idx, "baseNameOf", &mut defines);
    idx = insert_root_definition(idx, "builtins", &mut defines);
    idx = insert_root_definition(idx, "derivation", &mut defines);
    idx = insert_root_definition(idx, "dirOf", &mut defines);
    idx = insert_root_definition(idx, "fetchTarball", &mut defines);
    idx = insert_root_definition(idx, "import", &mut defines);
    idx = insert_root_definition(idx, "isNull", &mut defines);
    idx = insert_root_definition(idx, "map", &mut defines);
    idx = insert_root_definition(idx, "placeholder", &mut defines);
    idx = insert_root_definition(idx, "removeAttrs", &mut defines);
    idx = insert_root_definition(idx, "toString", &mut defines);

    (idx, defines)
}

/// The kind of scope that is defined
#[derive(Debug, PartialEq, Clone, Copy, Hash)]
pub enum ScopeKind {
    Root,
    With,
    LetIn,
    AttrSet,
    RecursiveAttrSet,
    Lambda,
}

/// Any error that occurs while analyzing scopes
#[derive(Debug, PartialEq, Clone)]
pub enum ScopeAnalysisError {
    /// This identifier has already been defined in the same binding
    ///
    /// The signature is: scope kind, identifier string, identifier location, location of previous definition
    AlreadyDefined(ScopeKind, String, TextRange, TextRange),
}

/// Scope refers to anything that defines a set of variables or attributes.
///
/// It is a superset of lexical nix scopes that includes attribute set scopes that do not use `rec`.
#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    kind: ScopeKind,
    defines: BTreeMap<String, Definition>,
    text_range: TextRange,
}

impl Scope {
    /// Get a definition by name in this scope (not including parent scopes).
    pub fn get_definition(&self, name: &str) -> Option<Definition> {
        if self.kind == ScopeKind::AttrSet {
            return None;
        }
        self.defines.get(name).cloned()
    }

    /// Returns true if the scope is able to define all its definitions
    ///
    /// This is false e.g. for `with`, as we cannot determine all variable names
    pub fn is_exhaustive(&self) -> bool {
        self.kind != ScopeKind::With
    }
}

/// This trait is implemented by any nodes that build up their own scope
trait DefinesScope {
    fn get_scope(
        &self,
        idx: usize,
        node: &SyntaxNode,
        scopes: &mut Vec<Scope>,
        errors: &mut Vec<ScopeAnalysisError>,
    ) -> usize;
}

impl DefinesScope for With {
    fn get_scope(
        &self,
        idx: usize,
        node: &SyntaxNode,
        scopes: &mut Vec<Scope>,
        _errors: &mut Vec<ScopeAnalysisError>,
    ) -> usize {
        scopes.push(Scope {
            kind: ScopeKind::With,
            defines: BTreeMap::new(),
            text_range: node.text_range(),
        });
        idx
    }
}

impl DefinesScope for LetIn {
    fn get_scope(
        &self,
        idx: usize,
        node: &SyntaxNode,
        scopes: &mut Vec<Scope>,
        errors: &mut Vec<ScopeAnalysisError>,
    ) -> usize {
        let mut defines = BTreeMap::new();
        let mut idx = populate_from_entries(ScopeKind::LetIn, self, &mut defines, errors, idx);
        for inherit in self.inherits() {
            for ident in inherit.idents() {
                idx = insert_into_defines(ScopeKind::LetIn, &mut defines, &ident, errors, idx);
            }
        }

        scopes.push(Scope {
            kind: ScopeKind::LetIn,
            defines,
            text_range: node.text_range(),
        });

        idx
    }
}

impl DefinesScope for AttrSet {
    fn get_scope(
        &self,
        idx: usize,
        node: &SyntaxNode,
        scopes: &mut Vec<Scope>,
        errors: &mut Vec<ScopeAnalysisError>,
    ) -> usize {
        let mut defines = BTreeMap::new();
        let scope_kind = if self.recursive() {
            ScopeKind::RecursiveAttrSet
        } else {
            ScopeKind::AttrSet
        };
        let mut idx = populate_from_entries(scope_kind, self, &mut defines, errors, idx);
        for inherit in self.inherits() {
            for ident in inherit.idents() {
                idx = insert_into_defines(ScopeKind::LetIn, &mut defines, &ident, errors, idx);
            }
        }
        scopes.push(Scope {
            kind: scope_kind,
            defines,
            text_range: node.text_range(),
        });

        idx
    }
}

impl DefinesScope for Lambda {
    fn get_scope(
        &self,
        mut idx: usize,
        node: &SyntaxNode,
        scopes: &mut Vec<Scope>,
        errors: &mut Vec<ScopeAnalysisError>,
    ) -> usize {
        let mut defines = BTreeMap::new();
        let arg_definition = self.arg().and_then(|arg| ParsedType::try_from(arg).ok());
        match arg_definition {
            Some(ParsedType::Ident(ident)) => {
                idx = insert_into_defines(ScopeKind::Lambda, &mut defines, &ident, errors, idx)
            }
            Some(ParsedType::Pattern(pattern)) => {
                for entry in pattern.entries() {
                    if let Some(ident) = entry.name() {
                        idx = insert_into_defines(
                            ScopeKind::Lambda,
                            &mut defines,
                            &ident,
                            errors,
                            idx,
                        )
                    }
                }
                if let Some(ident) = pattern.bind() {
                    idx = insert_into_defines(ScopeKind::Lambda, &mut defines, &ident, errors, idx)
                }
            }
            _ => {}
        }

        scopes.push(Scope {
            kind: ScopeKind::Lambda,
            defines,
            text_range: node.text_range(),
        });

        idx
    }
}

fn insert_into_defines(
    scope_kind: ScopeKind,
    defines: &mut BTreeMap<String, Definition>,
    ident: &Ident,
    errors: &mut Vec<ScopeAnalysisError>,
    idx: usize,
) -> usize {
    let name = ident.as_str().to_owned();
    if let Some(existing) = defines.get(&name) {
        errors.push(ScopeAnalysisError::AlreadyDefined(
            scope_kind,
            name,
            ident.node().text_range(),
            existing.text_range,
        ));
        idx
    } else {
        defines.insert(
            name.to_owned(),
            Definition {
                id: idx,
                name,
                text_range: ident.node().text_range(),
            },
        );
        idx + 1
    }
}

fn get_path_text_range(path: &[Ident]) -> TextRange {
    let from = path
        .first()
        .expect("should be a range")
        .node()
        .text_range()
        .start();
    let to = path
        .last()
        .expect("should be a range")
        .node()
        .text_range()
        .end();
    TextRange::from_to(from, to)
}

fn populate_already_defined_from_attrset(
    already_defined: &mut BTreeMap<String, (Vec<Ident>, Option<AttrSet>)>,
    prefix: &[Ident],
    set: AttrSet,
) {
    for entry in set.entries() {
        let path = entry
            .key()
            .map(|attr_name| {
                attr_name
                    .path()
                    .map(Ident::cast)
                    .map(|segment| match segment {
                        Some(s) => Ok(s),
                        None => Err("not an identifier"),
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .ok()
            })
            .unwrap_or_default();
        let value_as_attrset = entry.value().and_then(AttrSet::cast);

        if let Some(base_path) = path {
            let path = vec![prefix.to_owned(), base_path.clone()].concat();
            let path_str = path
                .iter()
                .map(|i| i.as_str().to_owned())
                .collect::<Vec<String>>()
                .join(".");

            already_defined.insert(path_str, (base_path, value_as_attrset.clone()));

            if let Some(value_as_attrset) = value_as_attrset {
                populate_already_defined_from_attrset(already_defined, &path, value_as_attrset)
            }
        }
    }
}

fn is_descendant_path(parent: &str, child: &str) -> bool {
    parent.starts_with(&format!("{}.", child))
}

fn populate_from_entries<T>(
    scope_kind: ScopeKind,
    set: &T,
    defines: &mut BTreeMap<String, Definition>,
    errors: &mut Vec<ScopeAnalysisError>,
    mut idx: usize,
) -> usize
where
    T: EntryHolder,
{
    let mut already_defined: BTreeMap<String, (Vec<Ident>, Option<AttrSet>)> = BTreeMap::new();
    for entry in set.entries() {
        let path = entry
            .key()
            .map(|attr_name| {
                attr_name
                    .path()
                    .map(Ident::cast)
                    .map(|segment| match segment {
                        Some(s) => Ok(s),
                        None => Err("not an identifier"),
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .ok()
            })
            .unwrap_or_default();
        let value_as_attrset = entry.value().and_then(AttrSet::cast);

        if let Some(path) = path {
            if let Some(ident) = path.get(0) {
                let path_str: String = path
                    .iter()
                    .map(|i| i.as_str().to_owned())
                    .collect::<Vec<String>>()
                    .join(".");
                if path.len() < 2 {
                    idx = insert_into_defines(scope_kind, defines, &ident, errors, idx);
                    if let Some(value_as_attrset) = value_as_attrset.clone() {
                        populate_already_defined_from_attrset(
                            &mut already_defined,
                            &path,
                            value_as_attrset,
                        )
                    }
                } else {
                    let existing = already_defined.iter().find(|p| {
                        p.0 == &path_str
                            || is_descendant_path(p.0, &path_str) && value_as_attrset.is_none()
                            || is_descendant_path(&path_str, p.0) && (p.1).1.is_none()
                    });
                    let ident_str = ident.as_str().to_owned();
                    if let Some(existing) = existing {
                        let text_range_error = get_path_text_range(&path);
                        let text_range_definition = get_path_text_range(&(existing.1).0);

                        errors.push(ScopeAnalysisError::AlreadyDefined(
                            scope_kind,
                            path_str.clone(),
                            text_range_error,
                            text_range_definition,
                        ))
                    } else if !defines.contains_key(&ident_str) {
                        idx = insert_into_defines(scope_kind, defines, &ident, errors, idx);
                        if let Some(value_as_attrset) = value_as_attrset.clone() {
                            populate_already_defined_from_attrset(
                                &mut already_defined,
                                &path,
                                value_as_attrset,
                            )
                        }
                    }
                }

                already_defined.insert(path_str, (path, value_as_attrset));
            }
        }
    }

    idx
}

/// Collect scopes from the AST, returns scopes and errors encountered during scope analysis
pub fn collect_scopes(ast: &rnix::AST) -> (Vec<Scope>, Vec<ScopeAnalysisError>) {
    let mut scopes = vec![];
    let mut errors_global = vec![];
    let idx = 0;
    let (mut idx, root_defines) = root_defines(idx);

    for node in ast.node().descendants() {
        match ParsedType::try_from(node.clone()) {
            Ok(ParsedType::With(with)) => {
                idx = with.get_scope(idx, &node, &mut scopes, &mut errors_global);
            }
            Ok(ParsedType::LetIn(let_in)) => {
                idx = let_in.get_scope(idx, &node, &mut scopes, &mut errors_global);
            }
            Ok(ParsedType::AttrSet(attrset)) => {
                idx = attrset.get_scope(idx, &node, &mut scopes, &mut errors_global);
            }
            Ok(ParsedType::Lambda(lambda)) => {
                idx = lambda.get_scope(idx, &node, &mut scopes, &mut errors_global);
            }
            _ => {}
        }
    }

    scopes.push(Scope {
        kind: ScopeKind::Root,
        defines: root_defines,
        text_range: ast.node().text_range(),
    });

    (scopes, errors_global)
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
    fn test_no_scopes() {
        run_snapshot_test("1")
    }

    #[test]
    fn test_scope_with() {
        run_snapshot_test("with builtins; 1")
    }

    #[test]
    fn test_scope_let_in_single_variable() {
        run_snapshot_test("let a = 1; in a")
    }

    #[test]
    fn test_scope_let_in_error_already_defined() {
        run_error_snapshot_test("let a = 1; a = 1; in a")
    }

    #[test]
    fn test_scope_let_in_single_nested_variable() {
        run_snapshot_test("let a.b.c = 1; in a")
    }

    #[test]
    fn test_scope_let_in_error_nested_already_defined() {
        run_error_snapshot_test("let a.b = 1; a.b = 1; in a")
    }

    #[test]
    fn test_scope_let_in_multiple_variable() {
        run_snapshot_test("let a = 1; b = a; in a + b")
    }

    #[test]
    fn test_scope_let_in_multiple_nested_variables() {
        run_snapshot_test("let a.b.c = 1; a.b.d = 2; in a")
    }

    #[test]
    fn test_scope_let_in_multiple_nested_variables_with_same_prefix() {
        run_snapshot_test("let a = 1; as.b = 2; in a")
    }

    #[test]
    fn test_scope_let_in_incremental_nested_building() {
        run_snapshot_test("let a = {}; a.b.c = 1; a.b = {}; in a.b.c")
    }

    #[test]
    fn test_scope_let_in_error_nested_parent_already_defined() {
        run_error_snapshot_test("let a.b = 1; a.b.c = 1; in a")
    }

    #[test]
    fn test_scope_let_in_error_nested_attr_already_defined_in_parent() {
        run_error_snapshot_test("let a = { b = 1; }; a.b = 1; in a")
    }

    #[test]
    fn test_scope_let_in_inherit() {
        run_snapshot_test("let a = 1; b = 2; inherit builtins; in a + b")
    }

    #[test]
    fn test_scope_let_in_error_inherit_already_defined() {
        run_error_snapshot_test("let builtins = 2; inherit builtins; in a + b")
    }

    #[test]
    fn test_scope_let_in_inherit_from() {
        run_snapshot_test("let a.b = 1; inherit (a) b; in a.b + b")
    }

    #[test]
    fn test_scope_let_in_error_inherit_from_already_defined() {
        run_error_snapshot_test("let a = 2; inherit (builtins) a; in a + b")
    }

    #[test]
    fn test_scope_attr_set() {
        run_snapshot_test(
            "a: d: {
            a = 1;
            b = a;
            c = 2;
            inherit d;
            inherit (d) e;
        }",
        )
    }

    #[test]
    fn test_scope_recursive_attr_set() {
        run_snapshot_test(
            "rec {
            a = 1;
            b = a;
            c = 2;
        }",
        )
    }

    #[test]
    fn test_scope_recursive_attr_set_error_already_defined() {
        run_error_snapshot_test(
            "rec {
            a = 1;
            b = a;
            b = 2;
        }",
        )
    }

    #[test]
    fn test_scope_lambda_simple() {
        run_snapshot_test("a: b: a + b")
    }

    #[test]
    fn test_scope_lambda_pattern() {
        run_snapshot_test("{a, b}: a + b")
    }

    #[test]
    fn test_scope_scope_lambda_pattern_error_already_defined() {
        run_error_snapshot_test("{a, a}: a + a")
    }

    #[test]
    fn test_scope_lambda_pattern_at() {
        run_snapshot_test("{a, b}@c: a + b + c.a")
    }

    #[test]
    fn test_scope_scope_lambda_pattern_at_error_already_defined() {
        run_error_snapshot_test("{a, b}@a: a + b + c.a")
    }

    #[test]
    fn test_scope_nested() {
        run_snapshot_test(
            "{a ? ''test'', b ? ''test''}: with import<nixpkgs>; let c = d: ''test''; in a + b + c",
        )
    }

    fn run_snapshot_test(nix_code: &str) {
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
        assert_debug_snapshot!(scopes);
        let scope_tree = InverseScopeTree::from_scopes(&scopes);
        assert_debug_snapshot!(scope_tree);
    }

    fn run_error_snapshot_test(nix_code: &str) {
        let output = Command::new("nix")
            .args(&["eval", &format!("({})", nix_code)])
            .output()
            .expect("failed to run nix");
        println!("{}", str::from_utf8(&output.stdout).unwrap());
        eprintln!("{}", str::from_utf8(&output.stderr).unwrap());
        assert_eq!(output.status.code(), Some(1));
        let parse_result = rnix::parse(nix_code);
        assert_eq!(parse_result.errors(), vec![]);
        let scopes = collect_scopes(&parse_result);
        assert_debug_snapshot!(scopes.1);
    }
}
