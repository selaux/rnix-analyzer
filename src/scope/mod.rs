use crate::CollectFromTree;
use id_arena::{Arena, Id as ArenaId};
use rnix::{
    types::{
        AttrSet, Dynamic, EntryHolder, Ident, Key, Lambda, LetIn, ParsedType, Str, TokenWrapper,
        TypedNode, With,
    },
    value::StrPart,
    SyntaxKind, SyntaxNode, TextRange, TextUnit,
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
            Some(ParsedType::Ident(ident)) => {
                let ident_str = ident.as_str().to_owned();
                insert_into_defines(
                    ScopeKind::Lambda,
                    &mut defines,
                    &ident_str,
                    ident.node().text_range(),
                    errors,
                    definition_arena,
                )
            }
            Some(ParsedType::Pattern(pattern)) => {
                for entry in pattern.entries() {
                    if let Some(ident) = entry.name() {
                        let ident_str = ident.as_str().to_owned();
                        insert_into_defines(
                            ScopeKind::Lambda,
                            &mut defines,
                            &ident_str,
                            ident.node().text_range(),
                            errors,
                            definition_arena,
                        )
                    }
                }
                if let Some(ident) = pattern.at() {
                    let ident_str = ident.as_str().to_owned();
                    insert_into_defines(
                        ScopeKind::Lambda,
                        &mut defines,
                        &ident_str,
                        ident.node().text_range(),
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
    name: &str,
    text_range: TextRange,
    errors: &mut Vec<ScopeAnalysisError>,
    arena: &mut Arena<Definition>,
) {
    if let Some(existing) = defines.get(name) {
        errors.push(ScopeAnalysisError::AlreadyDefined(
            scope_kind,
            name.to_owned(),
            text_range,
            arena[*existing].text_range,
        ));
    } else {
        let id = arena.alloc_with_id(|id| Definition {
            id,
            name: name.to_owned(),
            text_range,
        });
        defines.insert(name.to_owned(), id);
    }
}

#[derive(Clone)]
pub enum DefinitionPathSegment {
    Ident(Ident),
    Str(Str),
    InterpolatedString(Str),
    Dynamic(Dynamic),
}

impl DefinitionPathSegment {
    fn is_static(&self) -> bool {
        match self {
            Self::InterpolatedString(_) => false,
            Self::Dynamic(_) => false,
            _ => true,
        }
    }

    fn name(&self) -> Option<String> {
        match self {
            Self::Ident(i) => Some(i.as_str().to_owned()),
            Self::Str(s) => match &s.parts()[0] {
                StrPart::Literal(s) => Some(s.clone()),
                _ => None,
            },
            Self::InterpolatedString(_) | Self::Dynamic(_) => None,
        }
    }

    fn node(&self) -> &SyntaxNode {
        match self {
            Self::Ident(i) => i.node(),
            Self::Str(s) => s.node(),
            Self::InterpolatedString(s) => s.node(),
            Self::Dynamic(d) => d.node(),
        }
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        ParsedType::try_from(node).ok().and_then(|node| match node {
            ParsedType::Ident(ident) => Some(Self::Ident(ident)),
            ParsedType::Str(s) => {
                let parts = s.parts();
                let str_part = parts.get(0);
                if parts.len() == 1 {
                    if let Some(StrPart::Literal(_)) = str_part {
                        return Some(Self::Str(s));
                    }
                }
                Some(Self::InterpolatedString(s))
            }
            ParsedType::Dynamic(dynamic) => Some(Self::Dynamic(dynamic)),
            _ => None,
        })
    }
}

impl PartialEq for DefinitionPathSegment {
    fn eq(&self, other: &Self) -> bool {
        if let (Self::InterpolatedString(_), Self::InterpolatedString(_)) = (self, other) {
            false
        } else {
            self.name() == other.name()
        }
    }
}

#[derive(PartialEq, Clone)]
struct DefinitionPath(Vec<DefinitionPathSegment>);

impl DefinitionPath {
    fn first(&self) -> Option<&DefinitionPathSegment> {
        self.0.first()
    }

    fn last(&self) -> Option<&DefinitionPathSegment> {
        self.0.last()
    }

    fn iter(&self) -> impl Iterator<Item = &DefinitionPathSegment> {
        self.0.iter()
    }

    fn parent(&self) -> Option<Self> {
        let parent: Vec<_> = self
            .0
            .iter()
            .cloned()
            .take(self.0.len().saturating_sub(1))
            .collect();
        if parent.is_empty() {
            None
        } else {
            Some(Self(parent))
        }
    }

    fn join(&self, other: &Self) -> Self {
        let mut joined = self.0.clone();
        joined.append(&mut other.0.clone());
        Self(joined)
    }

    fn empty() -> Self {
        DefinitionPath(vec![])
    }

    fn is_static(&self) -> bool {
        self.0.iter().all(|segment| segment.is_static())
    }

    fn as_string(&self) -> String {
        self.0
            .iter()
            .map(|v| v.name().unwrap_or_else(|| "<dynamic>".to_owned()))
            .collect::<Vec<String>>()
            .join(".")
    }

    fn from_ident(key: Ident) -> Self {
        Self(vec![DefinitionPathSegment::Ident(key)])
    }

    fn from_key(key: Key) -> Option<Self> {
        let values: Vec<_> = key.path().filter_map(DefinitionPathSegment::cast).collect();
        if values.is_empty() {
            None
        } else {
            Some(Self(values))
        }
    }

    fn text_range(&self) -> Option<TextRange> {
        let from = self.first()?;
        let from = from.node().text_range().start();
        let to = self.last()?;
        let to = to.node().text_range().end();
        Some(TextRange::from_to(from, to))
    }
}

fn populate_definitions_from_entry_holder<T>(
    definitions: &mut BTreeMap<String, Vec<(DefinitionPath, Option<AttrSet>)>>,
    prefix: &DefinitionPath,
    set: &T,
) where
    T: EntryHolder,
{
    for entry in set.entries() {
        let path = entry.key().and_then(DefinitionPath::from_key);
        if let Some(path) = path {
            let path = prefix.join(&path);
            let value_as_attrset = entry.value().and_then(AttrSet::cast);
            let path_str = path.as_string();
            let entry = definitions.entry(path_str).or_default();
            entry.push((path.clone(), value_as_attrset.clone()));
            if let Some(value_as_attrset) = value_as_attrset.as_ref() {
                populate_definitions_from_entry_holder(definitions, &path, value_as_attrset)
            }
        }
    }
    for inherit in set.inherits() {
        for ident in inherit.idents() {
            let path = DefinitionPath::from_ident(ident);
            let path_str = path.as_string();
            let value_as_attrset = None;

            let entry = definitions.entry(path_str).or_default();
            entry.push((path.clone(), value_as_attrset.clone()));
        }
    }
}

fn insert_path_into_defines(
    scope_kind: ScopeKind,
    defines: &mut BTreeMap<String, DefinitionId>,
    errors: &mut Vec<ScopeAnalysisError>,
    arena: &mut Arena<Definition>,
    definition: &DefinitionPath,
) {
    if definition.is_static() {
        let value = definition
            .first()
            .expect("definition should have at least one element");
        let name = value
            .name()
            .expect("first element of static definition should have a name");
        if !defines.contains_key(&name) {
            insert_into_defines(
                scope_kind,
                defines,
                &name,
                value.node().text_range(),
                errors,
                arena,
            );
        }
    } else {
        let value = definition.iter().take_while(|v| v.is_static()).next();
        if let Some(value) = value {
            let name = value.name().expect("static value should have a name");
            if !defines.contains_key(&name) {
                insert_into_defines(
                    scope_kind,
                    defines,
                    &name,
                    value.node().text_range(),
                    errors,
                    arena,
                );
            }
        }
    }
}

fn ensure_nested_paths(
    scope_kind: ScopeKind,
    definitions: &BTreeMap<String, Vec<(DefinitionPath, Option<AttrSet>)>>,
    errors: &mut Vec<ScopeAnalysisError>,
) {
    for definition in definitions.values() {
        let definition = definition
            .first()
            .expect("there should be at least one definition");
        let mut outer_parent = definition.0.parent();
        while let Some(parent) = outer_parent.as_ref() {
            if parent.is_static() {
                let parent_path_str = parent.as_string();

                if let Some(parent_definitions) = definitions.get(&parent_path_str) {
                    let (parent_definition, parent_attrset) = parent_definitions
                        .first()
                        .expect("there should be at least one parent definition");
                    if parent_attrset.is_none() {
                        errors.push(ScopeAnalysisError::AlreadyDefined(
                            scope_kind,
                            parent_path_str.clone(),
                            parent
                                .text_range()
                                .expect("parent textrange should be a range"),
                            parent_definition
                                .last()
                                .expect("parent definition should have at least one element")
                                .node()
                                .text_range(),
                        ))
                    }
                }
            }

            outer_parent = parent.parent();
        }
    }
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
    let mut definitions: BTreeMap<String, Vec<(DefinitionPath, Option<AttrSet>)>> = BTreeMap::new();

    populate_definitions_from_entry_holder(&mut definitions, &DefinitionPath::empty(), set);

    ensure_nested_paths(scope_kind, &definitions, errors);
    for (path_str, value) in definitions.iter() {
        match value.len() {
            1 => {
                let definition = &value[0].0;
                insert_path_into_defines(scope_kind, defines, errors, arena, definition);
            }
            x if x > 1 => {
                let definition = &value[0].0;

                insert_path_into_defines(scope_kind, defines, errors, arena, definition);

                if definition.is_static() {
                    for duplicate_definition in value.iter().skip(1) {
                        let text_range_error = duplicate_definition
                            .0
                            .text_range()
                            .expect("duplicate definition should have a range");
                        let text_range_definition = definition
                            .0
                            .last()
                            .expect("existing path should not be empty")
                            .node()
                            .text_range();

                        errors.push(ScopeAnalysisError::AlreadyDefined(
                            scope_kind,
                            path_str.clone(),
                            text_range_error,
                            text_range_definition,
                        ))
                    }
                }
            }
            _ => {}
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
    fn test_scope_let_in_single_variable_as_string() {
        run_snapshot_test("let \"a\" = 1; in a")
    }

    #[test]
    fn test_scope_let_in_error_already_defined() {
        run_error_snapshot_test("let a = 1; a = 1; in a")
    }

    #[test]
    fn test_scope_let_in_error_already_defined_as_string() {
        run_error_snapshot_test("let a = 1; \"a\" = 1; in a")
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
    fn test_scope_let_in_error_nested_already_defined_as_string() {
        run_error_snapshot_test("let a.\"b\" = 1; \"a\".b = 1; in a")
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
    fn test_scope_let_in_error_nested_parent_already_defined_as_string() {
        run_error_snapshot_test("let a.\"b\" = 1; \"a\".b.c = 1; in a")
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
    fn test_scope_let_in_error_inherit_already_defined_as_string() {
        run_error_snapshot_test("let \"builtins\" = 2; inherit builtins; in builtins + builtins")
    }

    #[test]
    fn test_scope_let_in_inherit_from() {
        run_snapshot_test("let a.b = 1; inherit (a) b; in a.b + b")
    }

    #[test]
    fn test_scope_let_in_error_inherit_from_already_defined() {
        run_error_snapshot_test("let a = 2; inherit (builtins) a; in a + a")
    }

    #[ignore]
    #[test]
    fn test_scope_let_in_error_inherit_from_already_defined_as_string() {
        run_error_snapshot_test("let \"a\" = 2; inherit (builtins) \"a\"; in a + a")
    }

    #[test]
    fn test_scope_attr_set() {
        run_snapshot_test(
            "a: d: {
            a = 1;
            b = a;
            \"c\" = 2;
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
            \"c\" = 2;
        }",
        )
    }

    #[test]
    fn test_scope_attr_set_interpolated_paths() {
        run_snapshot_test(
            r#"a: b: {
            "${a}".b = 1;
            a."${b}" = 2;
            a."${a}".e = 3;
            a.${a}.f = 4;
        }"#,
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
    fn test_scope_recursive_attr_set_error_already_defined_as_string() {
        run_error_snapshot_test(
            "rec {
            a = 1;
            \"b\" = a;
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
