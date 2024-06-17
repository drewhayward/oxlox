
#[derive(Debug)]
pub enum OpCode {
    Return,
    Constant { index: usize },
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}
