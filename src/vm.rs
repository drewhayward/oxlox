use core::panic;
use std::collections::HashMap;
use std::slice::Iter;
use std::{fmt, u16};

use crate::heap::{GcHeap, GcRef};
use crate::object::{Object, ObjectType};

#[derive(Debug, Clone)]
pub enum LoxValue {
    Nil,
    Number(f64),
    Bool(bool),
    Object(GcRef<Object>),
}

impl LoxValue {
    fn is_falsey(&self) -> bool {
        match self {
            Self::Nil | Self::Bool(false) => true,
            _ => false,
        }
    }

    /// Helper function to access a lox value as an object. Returns None if LoxValue is not an
    /// object
    fn as_obj_type(&self) -> Option<&ObjectType> {
        match self {
            LoxValue::Object(obj) => Some(obj.as_obj_ref()),
            _ => None,
        }
    }
}

// Not sure how gross this is but I find myself doing this conversion a lot
impl TryFrom<LoxValue> for String {
    type Error = ();

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        match value.as_obj_type().ok_or(())? {
            ObjectType::String(value) => Ok(value.to_owned()),
        }
    }
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Number(number) => write!(f, "{}", number),
            LoxValue::Bool(value) => write!(f, "{}", value),
            LoxValue::Object(object) => match &object.value {
                ObjectType::String(value) => write!(f, "{}", value),
            },
        }
    }
}
impl PartialEq for LoxValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs == rhs,
            (Self::Nil, _) => true,
            (Self::Number(lhs), Self::Number(rhs)) => lhs == rhs,
            (Self::Object(lhs), Self::Object(rhs)) => match (&lhs.value, &rhs.value) {
                // If they are strings we can compare their pointers since all strings are interned
                (ObjectType::String(_), ObjectType::String(_)) => lhs.ptr_equal(rhs),
            },
            // If Value types don't match then they aren't equal.
            _ => false,
        }
    }
}

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
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub values: Vec<LoxValue>,
    pub line_nums: Vec<u32>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            line_nums: vec![],
            values: vec![],
        }
    }

    pub fn write(&mut self, byte: u8, line_num: u32) {
        self.code.push(byte);
        self.line_nums.push(line_num);
    }

    pub fn write_op(&mut self, op: OpCode, line_num: u32) {
        self.write(op as u8, line_num)
    }

    pub fn overwrite(&mut self, location: usize, value: u8) {
        if self.code.len() <= location {
            panic!("Tried to write to a location outside the code chunk.")
        }

        let _ = std::mem::replace(&mut self.code[location], value);
    }

    pub fn add_constant(&mut self, value: LoxValue) -> u8 {
        if self.values.len() >= <u8 as Into<usize>>::into(std::u8::MAX) + 1 {
            panic!("Attempted to add more than 256 constants to a code chunk")
        }

        self.values.push(value);
        (self.values.len() - 1) as u8
    }

    pub fn read_constant(&self, index: u8) -> LoxValue {
        return self.values[index as usize].clone();
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut byte_iter = self.code.iter().zip(self.line_nums.iter()).enumerate();
        while let Some((i, (op, line))) = byte_iter.next() {
            let op_code: OpCode = u8::try_into(*op).expect("Byte should be a valid OpCode");
            println!("{:04} {} {:?}", i, line, op_code);

            for _ in 0..op_code.arity() {
                let (i, (op, line)) = byte_iter.next().expect("Argument byte expected");

                println!("{:04} {} {:?}", i, line, op);
            }
        }
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    /// Tried to use a value of an incorrect type.
    TypeError(String),
    /// Attempted to divide by zero.
    DivideByZero,
    /// Code chunk ended without a return instruction.
    NoReturn,
}

#[derive(Debug)]
pub enum ErrorKind {
    CompileError,
    VmError(RuntimeError),
}

#[derive(Debug)]
pub struct VM {
    pub debug: bool,
    pub heap: GcHeap,
    stack: Vec<LoxValue>,
    globals: HashMap<String, LoxValue>,
}

impl VM {
    pub fn new() -> VM {
        VM {
            heap: GcHeap::new(),
            debug: false,
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
        // Instruction pointer to the op yet to be executed
        let mut ip = 0;

        while let Some(current_byte) = chunk.code.get(ip) {
            let op: OpCode = u8::try_into(*current_byte)
                .expect("Attempted to decode a byte which is not a valid OpCode");

            // Consider a debug output of op codes and stack state here as we go
            match op {
                OpCode::Return => {
                    let ret = self.stack.pop().expect("Should have a value to pop.");
                    println!("{:?}", ret);
                    return Ok(());
                }
                OpCode::Constant => {
                    let index = Self::next_byte(&mut ip, &chunk).expect("Expected index following constant op.");
                    let constant = chunk.read_constant(index);
                    self.stack.push(constant);
                }
                OpCode::Negate => {
                    let value = self.stack.pop().expect("Stack should have a value.");
                    if let LoxValue::Number(num_value) = value {
                        self.stack.push(LoxValue::Number(-num_value));
                    } else {
                        return Err(RuntimeError::TypeError(
                            "Operand should be a number.".to_string(),
                        ));
                    }
                }
                OpCode::Add => {
                    let rhs_value = self.stack.pop().expect("Should have an RHS value to add");
                    let lhs_value = self.stack.pop().expect("Should have an LHS value to add");

                    match (lhs_value, rhs_value) {
                        (LoxValue::Number(lhs_value), LoxValue::Number(rhs_value)) => {
                            self.stack.push(LoxValue::Number(lhs_value + rhs_value))
                        }
                        (LoxValue::Object(lhs_rc), LoxValue::Object(rhs_rc)) => {
                            match (lhs_rc.as_obj_ref(), rhs_rc.as_obj_ref()) {
                                (ObjectType::String(lhs_value), ObjectType::String(rhs_value)) => {
                                    let new_value = self.heap.allocate(ObjectType::String(
                                        lhs_value.clone() + rhs_value,
                                    ));

                                    self.stack.push(LoxValue::Object(new_value))
                                } //_ => {
                                  //    return Err(RuntimeError::TypeError(format!(
                                  //        "Cannot add objects {:?} and {:?}",
                                  //        lhs_rc, rhs_rc
                                  //    )))
                                  //}
                            }
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(
                                "Operands should be numbers.".to_string(),
                            ))
                        }
                    }
                }
                OpCode::Subtract => {
                    let rhs = self
                        .stack
                        .pop()
                        .expect("Should have an RHS value to subtract");
                    let lhs = self
                        .stack
                        .pop()
                        .expect("Should have an LHS value to subtract");

                    match (lhs, rhs) {
                        (LoxValue::Number(lhs_value), LoxValue::Number(rhs_value)) => {
                            self.stack.push(LoxValue::Number(lhs_value - rhs_value))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(
                                "Operands should be numbers.".to_string(),
                            ))
                        }
                    }
                }
                OpCode::Multiply => {
                    let rhs = self
                        .stack
                        .pop()
                        .expect("Should have an RHS value to multiply");
                    let lhs = self
                        .stack
                        .pop()
                        .expect("Should have an LHS value to multiply");
                    match (lhs, rhs) {
                        (LoxValue::Number(lhs_value), LoxValue::Number(rhs_value)) => {
                            self.stack.push(LoxValue::Number(lhs_value * rhs_value))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(
                                "Operands should be numbers.".to_string(),
                            ))
                        }
                    }
                }
                OpCode::Divide => {
                    let rhs = self
                        .stack
                        .pop()
                        .expect("Should have an RHS value to divide");
                    let lhs = self
                        .stack
                        .pop()
                        .expect("Should have an LHS value to divide");
                    match (lhs, rhs) {
                        (LoxValue::Number(lhs_value), LoxValue::Number(rhs_value)) => {
                            self.stack.push(LoxValue::Number(lhs_value / rhs_value))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(
                                "Operands should be numbers.".to_string(),
                            ))
                        }
                    }
                }
                OpCode::Nil => self.stack.push(LoxValue::Nil),
                OpCode::True => self.stack.push(LoxValue::Bool(true)),
                OpCode::False => self.stack.push(LoxValue::Bool(false)),
                OpCode::Not => match self.stack.pop() {
                    Some(value) => self.stack.push(LoxValue::Bool(value.is_falsey())),
                    None => panic!("Value should exist"),
                },
                OpCode::Equal => {
                    let rhs = self
                        .stack
                        .pop()
                        .expect("Should have an RHS value to divide");
                    let lhs = self
                        .stack
                        .pop()
                        .expect("Should have an LHS value to divide");
                    self.stack.push(LoxValue::Bool(lhs == rhs));
                }
                OpCode::Greater => {
                    let rhs = self
                        .stack
                        .pop()
                        .expect("Should have an RHS value to divide");
                    let lhs = self
                        .stack
                        .pop()
                        .expect("Should have an LHS value to divide");

                    match (lhs, rhs) {
                        (LoxValue::Number(lhs_value), LoxValue::Number(rhs_value)) => {
                            self.stack.push(LoxValue::Bool(lhs_value > rhs_value))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(
                                "Operands should be numbers.".to_string(),
                            ))
                        }
                    }
                }
                OpCode::Less => {
                    let rhs = self
                        .stack
                        .pop()
                        .expect("Should have an RHS value to divide");
                    let lhs = self
                        .stack
                        .pop()
                        .expect("Should have an LHS value to divide");

                    match (lhs, rhs) {
                        (LoxValue::Number(lhs_value), LoxValue::Number(rhs_value)) => {
                            self.stack.push(LoxValue::Bool(lhs_value < rhs_value))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(
                                "Operands should be numbers.".to_string(),
                            ))
                        }
                    }
                }
                OpCode::Print => {
                    let value = self.stack.pop().expect("should have a value to print");
                    println!("{value}")
                }
                OpCode::DefineGlobal => {
                    let name_index = Self::next_byte(&mut ip, &chunk).expect("Expected index following constant op.");
                    let name_value = chunk.read_constant(name_index);
                    let global_value = self
                        .stack
                        .pop()
                        .expect("Should have a value on the stack for global definition");

                    match name_value
                        .as_obj_type()
                        .expect("Stack value should be an object")
                    {
                        ObjectType::String(value) => {
                            self.globals.insert(value.to_owned(), global_value);
                        }
                        _ => unreachable!(
                            "Lox values should be defined with an index to a string constant."
                        ),
                    };
                }
                OpCode::GetGlobal => {
                    let name_index = Self::next_byte(&mut ip, &chunk)
                        .expect("Expected var name index following constant op.");
                    let name: String = chunk.read_constant(name_index).try_into().unwrap();
                    let value = self.globals.get(&name).unwrap();

                    self.stack.push(value.clone())
                }
                OpCode::SetGlobal => {
                    let name_index = Self::next_byte(&mut ip, &chunk)
                        .expect("Expected var name index following constant op.");
                    let name: String = chunk.read_constant(name_index).try_into().unwrap();

                    let value = self.stack.pop().expect("Value should exist to set.");
                    self.globals.insert(name, value);
                }
                OpCode::Pop => {
                    let _ = self.stack.pop();
                }
                OpCode::GetLocal => {
                    let stack_slot = Self::next_byte(&mut ip, &chunk).expect("Stack slot exists");
                    self.stack.push(
                        self.stack
                            .get(stack_slot as usize)
                            .expect("Local variable exists on the stack.")
                            .clone(),
                    )
                }
                OpCode::SetLocal => {
                    let stack_slot = Self::next_byte(&mut ip, &chunk).expect("Stack slot exists");
                    let new_value = self
                        .stack
                        .last()
                        .expect("New variable value exists on the stack.")
                        .clone();
                    let variable_ref = self
                        .stack
                        .get_mut(stack_slot as usize)
                        .expect("local variable exists");

                    *variable_ref = new_value;
                }
                OpCode::JumpIfFalse => {
                    let jump_condition = self.stack.last().expect("jump condition exists");
                    let jump_offset = VM::read_u16(&mut ip, chunk);

                    if jump_condition.is_falsey() {
                        ip += jump_offset as usize;
                    }
                }
                OpCode::Jump => {
                    let jump_offset = VM::read_u16(&mut ip, chunk);

                    ip += jump_offset as usize;
                }
            }

            ip += 1;
        }

        Err(RuntimeError::NoReturn)
    }

    fn next_byte(ip: &mut usize, chunk: &Chunk) -> Option<u8> {
        *ip += 1;
        chunk.code.get(*ip).copied()
    }

    /// Read a u16 from the code chunk
    fn read_u16(ip: &mut usize, chunk: &Chunk) -> u16 {
        let big_byte = Self::next_byte(ip, chunk).unwrap();
        let little_byte = Self::next_byte(ip, chunk).unwrap();

        ((big_byte as u16) << 8) + (little_byte as u16)
    }

    pub fn reset(&mut self) {
        self.stack.clear()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn chunk_can_write_ops() {
        let mut chunk = Chunk::new();
        let ops = vec![OpCode::Add, OpCode::Subtract, OpCode::Multiply];
        let op_bytes: Vec<_> = ops.iter().cloned().map(|op| op as u8).collect();

        for op in ops {
            chunk.write_op(op, 1);
        }

        assert_eq!(chunk.code, op_bytes)
    }

    #[test]
    fn chunk_reads_its_writes() {
        let mut c = Chunk::new();
        let value = 42.0;
        let index = c.add_constant(LoxValue::Number(value));
        assert_eq!(LoxValue::Number(value), c.read_constant(index));
    }

    #[test]
    #[should_panic]
    fn chunk_panics_for_more_than_256_constants() {
        let mut c = Chunk::new();
        let value = 42.0;

        for _ in 0..=255 + 1 {
            let _ = c.add_constant(LoxValue::Number(value));
        }
    }
}
