use crate::chunk::Chunk;
use std::fmt;

#[derive(Clone)]
pub struct Function {
    arity: u8,
    pub chunk: Chunk,
    name_id: u32,
}

impl Function {
    pub const fn new() -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name_id: 0,
        }
    }
}


// impl<'a> Object<'a> {
//     pub fn debug_string(&self) -> String {
//         match self {
//             Object::Str(s) => format!("\"{}\"", s),
//             _ => "Object".to_string(),
//         }
//     }
//     pub fn as_bool(&self) -> bool {
//         match self {
//             Object::Str(s) => !s.is_empty(),
//             _ => true,
//         }
//     }
// }

// impl<'a> PartialEq for Object<'a> {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Object::Str(a), Object::Str(b)) => a == b,
//             (Object::Fn{id: a, ..}, Object::Fn{id: b, ..}) => a == b,
//             _ => false,
//         }
//     }
// }
// impl<'a> Eq for Object<'a> {}

// impl<'a> fmt::Display for Object<'a> {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             Object::Str(s) => write!(f, "{}", s),
//             Object::Fn{id, ..} => {
//                 write!(f, "fn[{}]", id)
//             }
//         }
//     }
// }