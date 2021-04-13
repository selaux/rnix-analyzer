use std::collections::BTreeMap;

use id_arena::{Arena, Id as ArenaId};
use rnix::TextRange;

use super::DefinitionId;

/// Unique Id of a scope
pub(crate) type ScopeId = ArenaId<Scope>;

pub(crate) type ScopeArena = Arena<Scope>;

/// What kind of scope is referred to
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

/// Scope refers to anything that defines a set of variables or attributes.
///
/// It is a superset of lexical nix scopes that includes attribute set scopes that do not use `rec`.
#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    id: ScopeId,
    kind: ScopeKind,
    defines: BTreeMap<String, DefinitionId>,
    text_range: TextRange,
}

impl Scope {
    /// Create a new definition within an arena
    pub(crate) fn new_in_arena<'a>(arena: &'a mut ScopeArena, kind: ScopeKind, defines: BTreeMap<String, DefinitionId>, text_range: TextRange) -> &'a Scope {
        let id = arena.alloc_with_id(|id| Scope {
            id,
            kind,
            defines,
            text_range,
        });
        &arena[id]
    }

    pub (crate) fn id(&self) -> ScopeId {
        self.id
    }

    pub fn kind(&self) -> ScopeKind {
        self.kind
    }

    pub fn text_range(&self) -> TextRange {
        self.text_range
    }

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