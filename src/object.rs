use crate::vm::Chunk;

/// Lox heap-allocated object value
#[derive(Debug, PartialEq)]
pub enum HeapObject {
    String(String),
    Fn(Function),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub arity: i16,
    pub chunk: Chunk,
    // TODO: for now I'll just have Fn own its name
    pub name: String,
}
