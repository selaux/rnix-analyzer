use super::{NixType, TypeId};
use id_arena::Arena;

pub struct BaseTypes {
    pub unknown: TypeId,
    pub string: TypeId,
    pub path: TypeId,
    pub integer: TypeId,
    pub float: TypeId,
    pub boolean: TypeId,
    pub null: TypeId,
}

impl BaseTypes {
    pub fn register(type_arena: &mut Arena<NixType>) -> Self {
        let unknown = type_arena.alloc(NixType::Unknown);
        let string = type_arena.alloc(NixType::String);
        let path = type_arena.alloc(NixType::Path);
        let integer = type_arena.alloc(NixType::Integer);
        let float = type_arena.alloc(NixType::Float);
        let boolean = type_arena.alloc(NixType::Boolean);
        let null = type_arena.alloc(NixType::Null);
        BaseTypes {
            unknown,
            string,
            path,
            integer,
            float,
            boolean,
            null,
        }
    }
}
