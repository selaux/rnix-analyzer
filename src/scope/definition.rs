use id_arena::{Arena, Id as ArenaId};
use rnix::TextRange;

/// Unique Id of a definition
pub(crate) type DefinitionId = ArenaId<Definition>;

/// Arena where definitions are allocated
pub(crate) type DefinitionArena = Arena<Definition>;

/// A definition of a variable / attribute / parameter inside a [Scope](crate::Scope)
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Definition {
    /// Unique id of the definition
    id: DefinitionId,
    /// Name of the variable / attribute / parameter that is defined
    name: String,
    /// location of the definition
    text_range: TextRange,
}

impl Definition {
    /// Create a new definition within an arena
    pub(crate) fn new_in_arena<'a>(
        arena: &'a mut DefinitionArena,
        name: &str,
        text_range: TextRange,
    ) -> &'a Definition {
        let id = arena.alloc_with_id(|id| Definition {
            id,
            name: name.to_owned(),
            text_range,
        });
        &arena[id]
    }

    /// Get the unique id of the definition
    pub(crate) fn id(&self) -> DefinitionId {
        self.id
    }

    /// Get the name that is defined
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Text range where the definition happens
    pub fn text_range(&self) -> TextRange {
        self.text_range
    }
}
