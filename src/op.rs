#[derive(Debug)]
pub enum OpCode {
    Return,
    Constant { index: usize },
}
