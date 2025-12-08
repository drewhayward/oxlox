use crate::vm::Chunk;

/// Lox heap-allocated object value
#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    String(String),
    // Fn {
    //     arity: i16,
    //     chunk: Chunk,
    //     // TODO: for now I'll just have Fn own its name
    //     name: String,
    // },
}
