use std::ptr::NonNull;

/// Lox heap-allocated object
#[derive(Debug)]
pub struct Object {
    pub next: Option<NonNull<Object>>,
    pub value: ObjectType,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

/// Lox heap-allocated object value
#[derive(Debug, PartialEq, Eq)]
pub enum ObjectType {
    String(String),
}
