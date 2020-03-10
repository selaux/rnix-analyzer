use crate::CollectFromTree;
use id_arena::{Arena, Id as ArenaId};
use rnix::{
    types::{
        AttrSet, EntryHolder, Ident, Lambda, LetIn, ParsedType, TokenWrapper, TypedNode, With,
    },
    SyntaxKind, TextRange, TextUnit,
};
use std::collections::{BTreeMap, VecDeque};
use std::convert::TryFrom;

pub mod tree;
pub use tree::*;

/// Unique Id of a definition
pub type DefinitionId = ArenaId<Definition>;

/// A definition of a variable inside a Scope
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Definition {
    // Unique id of the definition
    pub id: DefinitionId,
    // name
    pub name: String,
    // location of the definition
    pub text_range: TextRange,
}

fn insert_root_definition(
    arena: &mut Arena<Definition>,
    name: &str,
    defines: &mut BTreeMap<String, DefinitionId>,
) {
    let definition_id = arena.alloc_with_id(|id| Definition {
        id,
        name: name.to_owned(),
        text_range: TextRange::from_to(TextUnit::from(0), TextUnit::from(0)),
    });
    defines.insert(name.to_owned(), definition_id);
}

// Returns the defines of the root node
pub fn root_defines(arena: &mut Arena<Definition>) -> BTreeMap<String, DefinitionId> {
    let mut defines = BTreeMap::new();

    insert_root_definition(arena, "true", &mut defines);
    insert_root_definition(arena, "false", &mut defines);
    insert_root_definition(arena, "null", &mut defines);
    insert_root_definition(arena, "throw", &mut defines);
    insert_root_definition(arena, "abort", &mut defines);
    insert_root_definition(arena, "baseNameOf", &mut defines);
    insert_root_definition(arena, "builtins", &mut defines);
    insert_root_definition(arena, "derivation", &mut defines);
    insert_root_definition(arena, "dirOf", &mut defines);
    insert_root_definition(arena, "fetchTarball", &mut defines);
    insert_root_definition(arena, "import", &mut defines);
    insert_root_definition(arena, "isNull", &mut defines);
    insert_root_definition(arena, "map", &mut defines);
    insert_root_definition(arena, "placeholder", &mut defines);
    insert_root_definition(arena, "removeAttrs", &mut defines);
    insert_root_definition(arena, "toString", &mut defines);

    defines
}

/// The kind of scope that is defined
#[derive(Debug, PartialEq, Clone, Copy, Hash)]
pub enum ScopeKind {
    /// The root scope at the top of the file, defines builtins
    Root,
    /// A scope defined with `with a;`
    With,
    /// At scope defined with `let a = 1; in ...`
    LetIn,
    /// A scope defined by `{ a = 1; }`
    AttrSet,
    /// A scope defined by `rec { a = 1; }`
    RecursiveAttrSet,
    /// A scope defined by `a: a`
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

/// Unique Id of a scope
pub type ScopeId = ArenaId<Scope>;

/// Scope refers to anything that defines a set of variables or attributes.
///
/// It is a superset of lexical nix scopes that includes attribute set scopes that do not use `rec`.
#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    pub id: ScopeId,
    pub kind: ScopeKind,
    defines: BTreeMap<String, DefinitionId>,
    pub text_range: TextRange,
}

impl Scope {
    /// Get definitions in this scope
    pub fn definitions(&self) -> impl Iterator<Item = &DefinitionId> {
        let simple_attrset = self.kind == ScopeKind::AttrSet;
        self.defines.values().filter(move |_| !simple_attrset)
    }
    /// Get a definition by name in this scope (not including parent scopes).
    pub fn definition(&self, name: &str) -> Option<&DefinitionId> {
        if self.kind == ScopeKind::AttrSet {
            return None;
        }
        self.defines.get(name)
    }

    /// Returns true if the scope is able to define all its definitions
    ///
    /// This is false e.g. for `with`, as we cannot determine all variable names
    pub fn is_exhaustive(&self) -> bool {
        self.kind != ScopeKind::With
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Scopes {
    definition_arena: Arena<Definition>,
    scope_arena: Arena<Scope>,
    scope_tree: InverseScopeTree,
    root_scope: ScopeId,
}

/// Result of scope analysis
impl Scopes {
    /// Returns a scope by id
    pub fn scope(&self, id: &ScopeId) -> Option<&Scope> {
        self.scope_arena.get(*id)
    }

    /// Returns all scopes
    pub fn scopes(&self) -> impl Iterator<Item = &Scope> {
        self.scope_arena.iter().map(|v| v.1)
    }

    /// Returns a definition by id
    pub fn definition(&self, id: &DefinitionId) -> Option<&Definition> {
        self.definition_arena.get(*id)
    }

    /// Returns all definitions
    pub fn definitions(&self) -> impl Iterator<Item = &Definition> {
        let definition_arena = &self.definition_arena;
        self.scopes()
            .flat_map(|v| v.definitions())
            .flat_map(move |id| definition_arena.get(*id))
    }

    /// Returns scopes that fully contain the passed range
    pub fn scopes_at(&self, range: TextRange) -> impl Iterator<Item = &Scope> {
        let scope_arena = &self.scope_arena;
        let leaf_scope = self.scope_tree.leaf_scopes.iter().find(|scopes| {
            let id = scopes.0.first().expect("more than one node");
            range.is_subrange(&self.scope_arena[*id].text_range)
        });
        leaf_scope
            .into_iter()
            .flat_map(|v| v.0.iter())
            .flat_map(move |id| scope_arena.get(*id))
    }

    /// Returns the root scope
    pub fn root_scope(&self) -> &Scope {
        &self.scope_arena[self.root_scope]
    }

    // Returns inverse scope tree
    pub fn inverse_scope_tree(&self) -> &InverseScopeTree {
        &self.scope_tree
    }
}

/// This trait is implemented by any nodes that build up their own scope
trait DefinesScope {
    fn get_scope(
        &self,
        definition_arena: &mut Arena<Definition>,
        scope_arena: &mut Arena<Scope>,
        errors: &mut Vec<ScopeAnalysisError>,
    ) -> ScopeId;
}

impl DefinesScope for With {
    fn get_scope<'a>(
        &self,
        _definition_arena: &mut Arena<Definition>,
        scope_arena: &'a mut Arena<Scope>,
        _errors: &mut Vec<ScopeAnalysisError>,
    ) -> ScopeId {
        scope_arena.alloc_with_id(|id| Scope {
            id,
            kind: ScopeKind::With,
            defines: BTreeMap::new(),
            text_range: self.node().text_range(),
        })
    }
}

impl DefinesScope for LetIn {
    fn get_scope(
        &self,
        definition_arena: &mut Arena<Definition>,
        scope_arena: &mut Arena<Scope>,
        errors: &mut Vec<ScopeAnalysisError>,
    ) -> ScopeId {
        let mut defines = BTreeMap::new();
        populate_from_entries(
            ScopeKind::LetIn,
            self,
            &mut defines,
            errors,
            definition_arena,
        );
        for inherit in self.inherits() {
            for ident in inherit.idents() {
                insert_into_defines(
                    ScopeKind::LetIn,
                    &mut defines,
                    &ident,
                    errors,
                    definition_arena,
                );
            }
        }

        scope_arena.alloc_with_id(|id| Scope {
            id,
            kind: ScopeKind::LetIn,
            defines,
            text_range: self.node().text_range(),
        })
    }
}

impl DefinesScope for AttrSet {
    fn get_scope(
        &self,
        definition_arena: &mut Arena<Definition>,
        scope_arena: &mut Arena<Scope>,
        errors: &mut Vec<ScopeAnalysisError>,
    ) -> ScopeId {
        let mut defines = BTreeMap::new();
        let scope_kind = if self.recursive() {
            ScopeKind::RecursiveAttrSet
        } else {
            ScopeKind::AttrSet
        };
        populate_from_entries(scope_kind, self, &mut defines, errors, definition_arena);
        for inherit in self.inherits() {
            for ident in inherit.idents() {
                insert_into_defines(
                    ScopeKind::LetIn,
                    &mut defines,
                    &ident,
                    errors,
                    definition_arena,
                );
            }
        }
        scope_arena.alloc_with_id(|id| Scope {
            id,
            kind: scope_kind,
            defines,
            text_range: self.node().text_range(),
        })
    }
}

impl DefinesScope for Lambda {
    fn get_scope(
        &self,
        definition_arena: &mut Arena<Definition>,
        scope_arena: &mut Arena<Scope>,
        errors: &mut Vec<ScopeAnalysisError>,
    ) -> ScopeId {
        let mut defines = BTreeMap::new();
        let arg_definition = self.arg().and_then(|arg| ParsedType::try_from(arg).ok());
        match arg_definition {
            Some(ParsedType::Ident(ident)) => insert_into_defines(
                ScopeKind::Lambda,
                &mut defines,
                &ident,
                errors,
                definition_arena,
            ),
            Some(ParsedType::Pattern(pattern)) => {
                for entry in pattern.entries() {
                    if let Some(ident) = entry.name() {
                        insert_into_defines(
                            ScopeKind::Lambda,
                            &mut defines,
                            &ident,
                            errors,
                            definition_arena,
                        )
                    }
                }
                if let Some(ident) = pattern.at() {
                    insert_into_defines(
                        ScopeKind::Lambda,
                        &mut defines,
                        &ident,
                        errors,
                        definition_arena,
                    )
                }
            }
            _ => {}
        }

        scope_arena.alloc_with_id(|id| Scope {
            id,
            kind: ScopeKind::Lambda,
            defines,
            text_range: self.node().text_range(),
        })
    }
}

fn insert_into_defines(
    scope_kind: ScopeKind,
    defines: &mut BTreeMap<String, DefinitionId>,
    ident: &Ident,
    errors: &mut Vec<ScopeAnalysisError>,
    arena: &mut Arena<Definition>,
) {
    let name = ident.as_str().to_owned();
    if let Some(existing) = defines.get(&name) {
        errors.push(ScopeAnalysisError::AlreadyDefined(
            scope_kind,
            name,
            ident.node().text_range(),
            arena[*existing].text_range,
        ));
    } else {
        let id = arena.alloc_with_id(|id| Definition {
            id,
            name: name.clone(),
            text_range: ident.node().text_range(),
        });
        defines.insert(name.to_owned(), id);
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
    set: &AttrSet,
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
            let path = vec![prefix, &base_path].concat();
            let path_str = path
                .iter()
                .map(|ident| ident.as_str())
                .collect::<Vec<_>>()
                .join(".");

            if let Some(value_as_attrset) = value_as_attrset.as_ref() {
                populate_already_defined_from_attrset(already_defined, &path, value_as_attrset)
            }
            already_defined.insert(path_str, (base_path, value_as_attrset));
        }
    }
}

fn is_descendant_path(parent: &str, child: &str) -> bool {
    parent.starts_with(&format!("{}.", child))
}

fn populate_from_entries<T>(
    scope_kind: ScopeKind,
    set: &T,
    defines: &mut BTreeMap<String, DefinitionId>,
    errors: &mut Vec<ScopeAnalysisError>,
    arena: &mut Arena<Definition>,
) where
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
                    insert_into_defines(scope_kind, defines, &ident, errors, arena);
                    if let Some(value_as_attrset) = value_as_attrset.as_ref() {
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
                        insert_into_defines(scope_kind, defines, &ident, errors, arena);
                        if let Some(value_as_attrset) = value_as_attrset.as_ref() {
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
}

pub fn defines_scope(kind: SyntaxKind) -> bool {
    kind == SyntaxKind::NODE_ATTR_SET
        || kind == SyntaxKind::NODE_LAMBDA
        || kind == SyntaxKind::NODE_LET_IN
        || kind == SyntaxKind::NODE_WITH
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct TrackScopesState {
    pub(crate) definition_arena: Arena<Definition>,
    pub(crate) scope_arena: Arena<Scope>,
    pub(crate) root_scope: ScopeId,
    pub(crate) current_scopes: VecDeque<Scope>,
    pub(crate) errors: Vec<ScopeAnalysisError>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct TrackScopes {
    state: TrackScopesState,
}

impl TrackScopes {
    pub fn new(ast: &rnix::AST) -> Self {
        let errors = vec![];
        let mut definition_arena = Arena::new();
        let mut scope_arena = Arena::new();
        let root_defines = root_defines(&mut definition_arena);
        let root_scope = scope_arena.alloc_with_id(|id| Scope {
            id,
            kind: ScopeKind::Root,
            defines: root_defines,
            text_range: ast.node().text_range(),
        });
        let current_scopes = vec![scope_arena[root_scope].clone()];

        TrackScopes {
            state: TrackScopesState {
                definition_arena,
                scope_arena,
                root_scope,
                current_scopes: VecDeque::from(current_scopes),
                errors,
            },
        }
    }
}

impl CollectFromTree<()> for TrackScopes {
    type State = TrackScopesState;
    type Result = Scopes;
    type Error = ScopeAnalysisError;

    fn enter_node(&mut self, _: (), node: &rnix::SyntaxNode) {
        if defines_scope(node.kind()) {
            let scope_id = match ParsedType::try_from(node.clone()) {
                Ok(ParsedType::With(with)) => with.get_scope(
                    &mut self.state.definition_arena,
                    &mut self.state.scope_arena,
                    &mut self.state.errors,
                ),
                Ok(ParsedType::LetIn(let_in)) => let_in.get_scope(
                    &mut self.state.definition_arena,
                    &mut self.state.scope_arena,
                    &mut self.state.errors,
                ),
                Ok(ParsedType::AttrSet(attrset)) => attrset.get_scope(
                    &mut self.state.definition_arena,
                    &mut self.state.scope_arena,
                    &mut self.state.errors,
                ),
                Ok(ParsedType::Lambda(lambda)) => lambda.get_scope(
                    &mut self.state.definition_arena,
                    &mut self.state.scope_arena,
                    &mut self.state.errors,
                ),
                _ => unreachable!(),
            };
            self.state
                .current_scopes
                .push_front(self.state.scope_arena[scope_id].clone());
        }
    }

    fn exit_node(&mut self, _: (), node: &rnix::SyntaxNode) {
        if defines_scope(node.kind()) {
            self.state.current_scopes.pop_front();
        }
    }

    fn state(&self) -> &Self::State {
        &self.state
    }

    fn result(self) -> (Self::Result, Vec<Self::Error>) {
        let scope_tree = InverseScopeTree::from_scopes(&self.state.scope_arena);
        (
            Scopes {
                definition_arena: self.state.definition_arena,
                scope_arena: self.state.scope_arena,
                scope_tree,
                root_scope: self.state.root_scope,
            },
            self.state.errors,
        )
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use crate::{AnalysisError, AnalysisResult};
    use insta::{assert_debug_snapshot, assert_display_snapshot};
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
        run_error_snapshot_test("let builtins = 2; inherit builtins; in builtins + builtins")
    }

    #[test]
    fn test_scope_let_in_inherit_from() {
        run_snapshot_test("let a.b = 1; inherit (a) b; in a.b + b")
    }

    #[test]
    fn test_scope_let_in_error_inherit_from_already_defined() {
        run_error_snapshot_test("let a = 2; inherit (builtins) a; in a + a")
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
        let result = AnalysisResult::from(&parse_result);
        let errors: Vec<AnalysisError> = result.errors().cloned().collect();
        assert_eq!(errors, vec![]);
        let mut scopes: Vec<_> = result.scopes().cloned().collect();
        let mut definitions: Vec<_> = result.definitions().cloned().collect();
        scopes.sort_by(|a, b| a.id.cmp(&b.id));
        definitions.sort_by(|a, b| a.id.cmp(&b.id));
        assert_display_snapshot!(format!(
            "{}\n=========\n{:#?}\n=========\n{:#?}\n=========\n{:#?}",
            nix_code,
            definitions,
            scopes,
            result.inverse_scope_tree()
        ));
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
        let result = AnalysisResult::from(&parse_result);
        let errors: Vec<AnalysisError> = result.errors().cloned().collect();
        assert_debug_snapshot!(errors);
    }
}
