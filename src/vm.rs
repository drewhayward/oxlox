use core::panic;
use std::cell::{Ref, RefMut};
use std::collections::HashMap;
use std::fmt::write;
use std::{fmt, u16};

use crate::heap::{Gc, GcHeap};
use crate::object::{Function, HeapObject};
use crate::vm::Value::Object;

#[derive(Debug)]
pub struct VM {
    pub debug: bool,
    pub heap: GcHeap,
    stack: Vec<Value>,
    frames: Vec<Frame>,
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> VM {
        VM {
            heap: GcHeap::new(),
            debug: false,
            stack: Vec::new(),
            frames: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, function: Gc<HeapObject>) -> Result<(), RuntimeError> {
        // Instruction pointer to the op yet to be executed
        // TODO: How to init frames ?
        self.frames.push(Frame {
            function: function,
            ip: 0,
        });

        while let Some(current_byte) = self.current_frame().read_u8() {
            let op: OpCode = u8::try_into(current_byte)
                .expect("Attempted to decode a byte which is not a valid OpCode");

            // Consider a debug output of op codes and stack state here as we go
            match op {
                OpCode::Return => {
                    let ret = self.stack.pop().expect("Should have a value to pop.");
                    println!("{:?}", ret);
                    return Ok(());
                }
                OpCode::Constant => {
                    let index = self
                        .current_frame()
                        .read_u8()
                        .expect("Expected index following constant op.");
                    let constant = self.current_frame().read_constant(index);
                    self.stack.push(constant);
                }
                OpCode::Negate => {
                    let value = self.stack.pop().expect("Stack should have a value.");
                    if let Value::Number(num_value) = value {
                        self.stack.push(Value::Number(-num_value));
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
                        (Value::Number(lhs_value), Value::Number(rhs_value)) => {
                            self.stack.push(Value::Number(lhs_value + rhs_value))
                        }
                        (Value::Object(lhs_rc), Value::Object(rhs_rc)) => {
                            match (&*lhs_rc.borrow(), &*rhs_rc.borrow()) {
                                (HeapObject::String(lhs_value), HeapObject::String(rhs_value)) => {
                                    let new_value = self.heap.allocate(HeapObject::String(
                                        lhs_value.clone() + rhs_value,
                                    ));

                                    self.stack.push(Value::Object(new_value))
                                }
                                _ => {
                                    return Err(RuntimeError::TypeError(format!(
                                        "Cannot add objects {:?} and {:?}",
                                        lhs_rc, rhs_rc
                                    )))
                                }
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
                        (Value::Number(lhs_value), Value::Number(rhs_value)) => {
                            self.stack.push(Value::Number(lhs_value - rhs_value))
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
                        (Value::Number(lhs_value), Value::Number(rhs_value)) => {
                            self.stack.push(Value::Number(lhs_value * rhs_value))
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
                        (Value::Number(lhs_value), Value::Number(rhs_value)) => {
                            self.stack.push(Value::Number(lhs_value / rhs_value))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(
                                "Operands should be numbers.".to_string(),
                            ))
                        }
                    }
                }
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::False => self.stack.push(Value::Bool(false)),
                OpCode::Not => match self.stack.pop() {
                    Some(value) => self.stack.push(Value::Bool(value.is_falsey())),
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
                    self.stack.push(Value::Bool(lhs == rhs));
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
                        (Value::Number(lhs_value), Value::Number(rhs_value)) => {
                            self.stack.push(Value::Bool(lhs_value > rhs_value))
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
                        (Value::Number(lhs_value), Value::Number(rhs_value)) => {
                            self.stack.push(Value::Bool(lhs_value < rhs_value))
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
                    let name_index = self
                        .current_frame()
                        .read_u8()
                        .expect("Expected index following constant op.");
                    let name_value = self.current_frame().read_constant(name_index);
                    let name_obj = name_value
                        .as_obj_type()
                        .expect("Stack value should be an object");
                    let global_value = self
                        .stack
                        .pop()
                        .expect("Should have a value on the stack for global definition");

                    match &*name_obj.borrow() {
                        HeapObject::String(value) => {
                            self.globals.insert(value.to_owned(), global_value);
                        }
                        _ => unreachable!(
                            "Lox values should be defined with an index to a string constant."
                        ),
                    };
                }
                OpCode::GetGlobal => {
                    let name_index = self
                        .current_frame()
                        .read_u8()
                        .expect("Expected var name index following constant op.");
                    let name_value = self
                        .current_frame()
                        .read_constant(name_index);
                    let name = name_value
                        .borrow_string()
                        .unwrap();
                    let value = self.globals.get(&*name).unwrap();

                    self.stack.push(value.clone())
                }
                OpCode::SetGlobal => {
                    let name_index = self
                        .current_frame()
                        .read_u8()
                        .expect("Expected var name index following constant op.");
                    let name_value = self
                        .current_frame()
                        .read_constant(name_index);
                    let name = name_value
                        .borrow_string()
                        .unwrap();

                    let value = self.stack.pop().expect("Value should exist to set.");
                    self.globals.insert(name.to_string(), value);
                }
                OpCode::Pop => {
                    let _ = self.stack.pop();
                }
                OpCode::GetLocal => {
                    let stack_slot = self.current_frame().read_u8().expect("Stack slot exists");
                    self.stack.push(
                        self.stack
                            .get(stack_slot as usize)
                            .expect("Local variable exists on the stack.")
                            .clone(),
                    )
                }
                OpCode::SetLocal => {
                    let stack_slot = self.current_frame().read_u8().expect("Stack slot exists");
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
                    let jump_offset = self.current_frame().read_u16() as usize;
                    let jump_condition = self.stack.last().expect("jump condition exists");

                    if jump_condition.is_falsey() {
                        self.current_frame().ip += jump_offset;
                    }
                }
                OpCode::Jump => {
                    let jump_offset = self.current_frame().read_u16() as usize;
                    self.current_frame().ip += jump_offset;
                }
                OpCode::Loop => {
                    let jump_offset = self.current_frame().read_u16() as usize;
                    self.current_frame().ip -= jump_offset;
                }
                OpCode::Call => {
                    let arg_count = self.current_frame().read_u8().expect("");

                    // Lookup the object which should be a function and handle exceptions to that
                    // Call the function object
                    // - add a stack frame to `frames` and set ip appropriately
                }
            }
        }

        Err(RuntimeError::NoReturn)
    }

    fn current_frame(&mut self) -> &mut Frame {
        self.frames.last_mut().expect("SHould have frame")
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Number(f64),
    Bool(bool),
    Object(Gc<HeapObject>),
}

impl Value {
    fn is_falsey(&self) -> bool {
        match self {
            Self::Nil | Self::Bool(false) => true,
            _ => false,
        }
    }

    fn as_obj_type(&self) -> Option<&Gc<HeapObject>> {
        if let Value::Object(gc) = self {
            Some(&gc)
        } else {
            None
        }
    }

    fn borrow_string(&self) -> Option<Ref<'_, str>> {
        let Value::Object(object) = self else {
            return None;
        };

        Ref::filter_map(object.borrow(), |obj| match obj {
            HeapObject::String(s) => Some(s.as_str()),
            _ => None,
        })
        .ok()
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Number(number) => write!(f, "{}", number),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Object(object) => match &*object.borrow() {
                HeapObject::String(value) => write!(f, "{}", value),
                HeapObject::Fn(func) => write!(f, "{}<{}>", func.name, func.arity),
            },
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs == rhs,
            (Self::Nil, _) => true,
            (Self::Number(lhs), Self::Number(rhs)) => lhs == rhs,
            (Self::Object(lhs), Self::Object(rhs)) => match (&*lhs.borrow(), &*rhs.borrow()) {
                (l, r) => l == r, // TODO: Bring back string interning
                                  // If they are strings we can compare their pointers since all strings are interned
                                  // (HeapObject::String(_), HeapObject::String(_)) => lhs.ptr_equal(rhs),
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

#[derive(Debug, PartialEq)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub values: Vec<Value>,
    /// Source code line number for the code byte. Matches up with [Chunk::code]
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

    pub fn add_constant(&mut self, value: Value) -> u8 {
        if self.values.len() >= <u8 as Into<usize>>::into(std::u8::MAX) + 1 {
            panic!("Attempted to add more than 256 constants to a code chunk")
        }

        self.values.push(value);
        (self.values.len() - 1) as u8
    }

    pub fn read_constant(&self, index: u8) -> Value {
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

/// Manages the ip for a single code chunk.
#[derive(Debug)]
pub struct Frame {
    function: Gc<HeapObject>,
    /// Code pointer into the function's [Chunk]. Always points to the next unread byte
    ip: usize,
}

impl Frame {
    fn try_new(function: Gc<HeapObject>) -> Result<Frame, ()> {
        // validate that heap ref points to a function once here so we can just unwrap in the
        // helpers. Assumes refs don't change type
        let is_func = matches!(*function.borrow(), HeapObject::Fn(_));

        if is_func {
            Ok(Self { function, ip: 0 })
        } else {
            Err(())
        }
    }

    /// Read the next chunk byte and advance the IP
    fn read_u8(&mut self) -> Option<u8> {
        self.borrow_function().chunk.code.get(self.ip).copied()
    }

    /// Read the next u16 from the chunk that IP is pointing to
    fn read_u16(&mut self) -> u16 {
        let big_byte = Self::read_u8(self).expect("first byte should exist");
        let little_byte = Self::read_u8(self).expect("second byte should exist");

        ((big_byte as u16) << 8) + (little_byte as u16)
    }

    fn borrow_function(&self) -> Ref<Function> {
        Ref::map(self.function.borrow(), |heap_func| match heap_func {
            HeapObject::Fn(func) => func,
            _ => panic!("Heap ref should point to Function by construction"),
        })
    }

    fn borrow_function_mut(&self) -> RefMut<Function> {
        RefMut::map(self.function.borrow_mut(), |heap_func| match heap_func {
            HeapObject::Fn(func) => func,
            _ => panic!("Heap ref should point to Function by construction"),
        })
    }

    fn read_constant(&self, index: u8) -> Value {
        let func = self.borrow_function();
        func.chunk.read_constant(index)
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
        let index = c.add_constant(Value::Number(value));
        assert_eq!(Value::Number(value), c.read_constant(index));
    }

    #[test]
    #[should_panic]
    fn chunk_panics_for_more_than_256_constants() {
        let mut c = Chunk::new();
        let value = 42.0;

        for _ in 0..=255 + 1 {
            let _ = c.add_constant(Value::Number(value));
        }
    }
}
