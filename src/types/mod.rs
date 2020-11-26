use crate::{utils::Stack, CollectFromTree, DefinitionId, Reference, Scope, VariableId};
use id_arena::{Arena, Id as ArenaId};
use rnix::{types::ParsedType, value::Value, SyntaxKind};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::convert::TryFrom;
use std::hash::Hash;
use std::iter::FromIterator;

mod base;

use base::BaseTypes;

/// Unique Id of a type
pub type TypeId = ArenaId<NixType>;

pub type TypeError = ();

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum NixType {
    Unknown,
    String,
    Path,
    Integer,
    Float,
    Boolean,
    Null,
    List(TypeId),
    AttrSet {
        attrs: BTreeMap<String, TypeId>,
        exhaustive: bool,
    },
    Lambda {
        arguments: BTreeMap<String, TypeId>,
        at: TypeId,
        returns: TypeId,
    },
    Union(BTreeSet<TypeId>),
}

impl NixType {
    pub(crate) fn to_type_descriptor(
        &self,
        type_arena: &Arena<NixType>,
        current_indent: usize,
    ) -> String {
        match self {
            NixType::Unknown => "unknown".to_owned(),
            NixType::String => "string".to_owned(),
            NixType::Path => "path".to_owned(),
            NixType::Integer => "int".to_owned(),
            NixType::Float => "float".to_owned(),
            NixType::Boolean => "bool".to_owned(),
            NixType::Null => "null".to_owned(),
            NixType::List(type_id) => format!(
                "list({})",
                type_arena[*type_id].to_type_descriptor(type_arena, current_indent)
            ),
            NixType::Union(types) => {
                let mut v: Vec<String> = types
                    .iter()
                    .map(|ty| type_arena[*ty].to_type_descriptor(type_arena, current_indent))
                    .collect();
                v.sort();
                v.join(" | ")
            }
            // TODO
            NixType::AttrSet { .. } => "set()".to_owned(),
            NixType::Lambda { .. } => "lambda()".to_owned(),
        }
    }
}

impl Default for NixType {
    fn default() -> Self {
        NixType::Unknown
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Types {
    type_arena: Arena<NixType>,
    type_for_definition: BTreeMap<DefinitionId, TypeId>,
    export_type: TypeId,
}

impl Types {
    pub fn nixtype_description(&self, type_id: &TypeId) -> Option<String> {
        self.nixtype(type_id)
            .map(|t| t.to_type_descriptor(&self.type_arena, 0))
    }

    /// Returns a scope by id
    pub fn nixtype(&self, id: &TypeId) -> Option<&NixType> {
        self.type_arena.get(*id)
    }

    /// Returns a scope by id
    pub fn export_nixtype(&self) -> &TypeId {
        &self.export_type
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ListContext {
    types: Vec<NixType>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum TrackTypesContext {
    Root,
    None,
    List(ListContext),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct TrackTypesDependencies<'a, 'b> {
    references: &'a BTreeMap<VariableId, Reference>,
    current_scopes: &'b Stack<Scope>,
}

impl TrackTypesDependencies<'_, '_> {
    pub fn new<'a, 'b>(
        references: &'a BTreeMap<VariableId, Reference>,
        current_scopes: &'b Stack<Scope>,
    ) -> TrackTypesDependencies<'a, 'b> {
        TrackTypesDependencies {
            references,
            current_scopes,
        }
    }
}

pub(crate) struct TrackTypesState {
    type_arena: Arena<NixType>,
    type_hashes: HashMap<NixType, TypeId>,
    base_types: BaseTypes,
    type_for_definition: BTreeMap<DefinitionId, TypeId>,
    export_type: TypeId,
    contexts: Stack<TrackTypesContext>,
}

impl TrackTypesState {
    fn get_or_insert_type(&mut self, typ: NixType) -> TypeId {
        if let Some(typ) = self.type_hashes.get(&typ) {
            *typ
        } else {
            self.type_arena.alloc(typ)
        }
    }
}

impl Default for TrackTypesState {
    fn default() -> Self {
        let mut type_arena = Arena::new();
        let base_types = BaseTypes::register(&mut type_arena);
        let export_type = base_types.unknown;

        TrackTypesState {
            type_arena,
            type_hashes: HashMap::new(),
            base_types,
            type_for_definition: BTreeMap::new(),
            export_type,
            contexts: Stack::default(),
        }
    }
}

#[derive(Default)]
pub(crate) struct TrackTypes {
    state: TrackTypesState,
}

impl TrackTypes {
    pub fn new() -> Self {
        Default::default()
    }
}

impl<'a, 'b> CollectFromTree<TrackTypesDependencies<'a, 'b>> for TrackTypes {
    type State = TrackTypesState;
    type Result = Types;
    type Error = TypeError;

    fn enter_node(&mut self, _: TrackTypesDependencies<'a, 'b>, node: &rnix::SyntaxNode) {
        println!("Enter {:?}", node.kind());
        match node.kind() {
            SyntaxKind::NODE_ROOT => self.state.contexts.push_front(TrackTypesContext::Root),
            SyntaxKind::NODE_LIST => self
                .state
                .contexts
                .push_front(TrackTypesContext::List(ListContext { types: vec![] })),
            _ => self.state.contexts.push_front(TrackTypesContext::None),
        }
    }

    fn exit_node(&mut self, _: TrackTypesDependencies<'a, 'b>, node: &rnix::SyntaxNode) {
        let state = &mut self.state;
        let context = state.contexts.pop_front();
        println!("Exit {:?}", node.kind());
        let parsed_type = ParsedType::try_from(node.clone());
        let current_type_id = match parsed_type {
            Ok(ParsedType::Root(_)) => state.base_types.unknown,
            Ok(ParsedType::Value(v)) => match v.to_value() {
                Ok(Value::Float(_)) => state.base_types.float,
                Ok(Value::Integer(_)) => state.base_types.integer,
                Ok(Value::Path(_, _)) => state.base_types.path,
                Ok(Value::String(_)) => state.base_types.string,
                _ => unreachable!(),
            },
            Ok(ParsedType::Str(_)) => state.base_types.string,
            Ok(ParsedType::List(_)) => match context {
                Some(TrackTypesContext::List(context)) => {
                    let types_hashset = BTreeSet::from_iter(context.types.into_iter());
                    let ids: BTreeSet<_> = types_hashset
                        .into_iter()
                        .map(|typ| state.get_or_insert_type(typ))
                        .collect();
                    let type_id = match ids.len() {
                        0 => state.base_types.unknown,
                        1 => ids.into_iter().next().unwrap(),
                        _ => state.get_or_insert_type(NixType::Union(ids)),
                    };
                    state.get_or_insert_type(NixType::List(type_id))
                }
                c => {
                    log::warn!("Exited list node, but found non-list-context: {:?}, returning list(unknown) type", c);
                    state.get_or_insert_type(NixType::List(state.base_types.unknown))
                }
            },
            Ok(_) => {
                log::warn!(
                    "Found non-inferrable node {:?}, returning unknown type",
                    node
                );
                state.base_types.unknown
            }
            Err(_) => {
                log::warn!(
                    "Error parsing into ParsedType for node {:?}, returning unknown type",
                    node
                );
                state.base_types.unknown
            }
        };
        let current_type = &state.type_arena[current_type_id];
        let parent_context = state.contexts.get_mut(0);
        match parent_context {
            Some(TrackTypesContext::Root) => state.export_type = current_type_id,
            Some(TrackTypesContext::List(parent_context)) => {
                parent_context.types.push(current_type.clone());
            }
            _ => {}
        }
    }

    fn state(&self) -> &Self::State {
        &self.state
    }

    fn result(self) -> (Self::Result, Vec<Self::Error>) {
        (
            Types {
                type_arena: self.state.type_arena,
                type_for_definition: self.state.type_for_definition,
                export_type: self.state.export_type,
            },
            vec![],
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{AnalysisError, AnalysisResult};
    use insta::assert_display_snapshot;
    use std::process::Command;
    use std::str;

    #[test]
    fn test_integer_export_nixtype() {
        assert_export_nixtype_snapshot("1");
    }

    #[test]
    fn test_float_export_nixtype() {
        assert_export_nixtype_snapshot("1.0");
    }

    #[test]
    fn test_path_export_nixtype() {
        assert_export_nixtype_snapshot("./.gitignore");
    }

    #[test]
    fn test_string_export_nixtype() {
        assert_export_nixtype_snapshot("\"\"");
    }

    #[test]
    fn test_empty_array_export_nixtype() {
        assert_export_nixtype_snapshot("[]");
    }

    #[test]
    fn test_integer_array_export_nixtype() {
        assert_export_nixtype_snapshot("[ 1 2 3 ]");
    }

    #[test]
    fn test_mixed_array_export_nixtype() {
        assert_export_nixtype_snapshot("[ 1 2.0 \"3\" \"1\" 2 3.0]");
    }

    fn assert_export_nixtype_snapshot(nix_code: &str) {
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
        let export_nixtype = result.export_nixtype();
        let description = result
            .nixtype_description(export_nixtype)
            .expect("should be there");
        assert_display_snapshot!(format!("{}\n=========\n{}", nix_code, description));
    }
}
