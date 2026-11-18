use crate::vm::{Chunk, RuntimeError, Value};

/// Lox heap-allocated object value
#[derive(Debug, PartialEq)]
pub enum HeapObject {
    String(String),
    Fn(Function),
    Native(NativeFunction),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub arity: i16,
    pub chunk: Chunk,
    // TODO: for now I'll just have Fn own its name
    pub name: String,
}

/// A host function receives arguments in source order and returns one Lox value.
/// The VM owns argument-count validation and call-stack cleanup.
pub type NativeFn = fn(&[Value]) -> Result<Value, RuntimeError>;

#[derive(Debug)]
pub struct NativeFunction {
    pub arity: u8,
    pub name: String,
    pub function: NativeFn,
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        // Native objects compare by identity, not by Rust function addresses.
        std::ptr::eq(self, other)
    }
}
