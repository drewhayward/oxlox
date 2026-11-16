#[derive(Debug, PartialEq, Clone)]
pub enum OpCode {
    Return,
    Constant,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Nil,
    True,
    False,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    /// Discard a value off the stack
    Pop,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    Jump,
    Loop,
    Call,
}

impl OpCode {
    // Returns the number of arguments this opcode expects in the bytecode
    fn arity(&self) -> i32 {
        match *self {
            Self::Constant => 1,
            Self::GetGlobal => 1,
            Self::SetGlobal => 1,
            Self::GetLocal => 1,
            Self::SetLocal => 1,
            Self::DefineGlobal => 1,
            Self::JumpIfFalse => 2,
            Self::Jump => 2,
            Self::Loop => 2,
            _ => 0,
        }
    }
}

// Need to be able to cast bytes back into op codes
// Could use a crate for this but we'll avoid the dependency for now
impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            x if x == OpCode::Return as u8 => Ok(OpCode::Return),
            x if x == OpCode::Constant as u8 => Ok(OpCode::Constant),
            x if x == OpCode::Negate as u8 => Ok(OpCode::Negate),
            x if x == OpCode::Add as u8 => Ok(OpCode::Add),
            x if x == OpCode::Subtract as u8 => Ok(OpCode::Subtract),
            x if x == OpCode::Multiply as u8 => Ok(OpCode::Multiply),
            x if x == OpCode::Divide as u8 => Ok(OpCode::Divide),
            x if x == OpCode::Nil as u8 => Ok(OpCode::Nil),
            x if x == OpCode::True as u8 => Ok(OpCode::True),
            x if x == OpCode::False as u8 => Ok(OpCode::False),
            x if x == OpCode::Not as u8 => Ok(OpCode::Not),
            x if x == OpCode::Equal as u8 => Ok(OpCode::Equal),
            x if x == OpCode::Greater as u8 => Ok(OpCode::Greater),
            x if x == OpCode::Less as u8 => Ok(OpCode::Less),
            x if x == OpCode::Print as u8 => Ok(OpCode::Print),
            x if x == OpCode::DefineGlobal as u8 => Ok(OpCode::DefineGlobal),
            x if x == OpCode::GetGlobal as u8 => Ok(OpCode::GetGlobal),
            x if x == OpCode::SetGlobal as u8 => Ok(OpCode::SetGlobal),
            x if x == OpCode::GetLocal as u8 => Ok(OpCode::GetLocal),
            x if x == OpCode::SetLocal as u8 => Ok(OpCode::SetLocal),
            x if x == OpCode::Pop as u8 => Ok(OpCode::Pop),
            x if x == OpCode::JumpIfFalse as u8 => Ok(OpCode::JumpIfFalse),
            x if x == OpCode::Jump as u8 => Ok(OpCode::Jump),
            x if x == OpCode::Loop as u8 => Ok(OpCode::Loop),
            _ => Err(()),
        }
    }
}
