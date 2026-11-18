use core::panic;
use std::cell::{Ref, RefMut};
use std::collections::HashMap;
use std::{fmt, u16};

use crate::heap::{Gc, GcHeap};
use crate::object::{Function, HeapObject};

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

    pub fn interpret(&mut self, function: Gc<HeapObject>) -> Result<Option<Value>, RuntimeError> {
        // Init the top level script callframe
        self.frames.push(Frame {
            function: function,
            ip: 0,
            stack_start: 0,
        });

        while let Some(current_byte) = self.current_frame().read_u8() {
            let op: OpCode = u8::try_into(current_byte)
                .expect("Attempted to decode a byte which is not a valid OpCode");

            // Consider a debug output of op codes and stack state here as we go
            match op {
                OpCode::Return => {
                    let frame_return = self.stack.pop().expect("Should have a value to pop.");
                    // Pop off the frame locals
                    if let Some(frame) = self.frames.pop() { 
                        while self.stack.len() >= frame.stack_start {
                            self.stack.pop();
                        }
                    }
                    if self.frames.is_empty() {
                        return Ok(Some(frame_return));
                    }
                    self.stack.push(frame_return);
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
                    let name_value = self.current_frame().read_constant(name_index);
                    let name = name_value.borrow_string().unwrap();
                    let value = self.globals.get(&*name).unwrap();

                    self.stack.push(value.clone())
                }
                OpCode::SetGlobal => {
                    let name_index = self
                        .current_frame()
                        .read_u8()
                        .expect("Expected var name index following constant op.");
                    let name_value = self.current_frame().read_constant(name_index);
                    let name = name_value.borrow_string().unwrap();

                    let value = self.stack.pop().expect("Value should exist to set.");
                    self.globals.insert(name.to_string(), value);
                }
                OpCode::Pop => {
                    let _ = self.stack.pop();
                }
                OpCode::GetLocal => {
                    let frame_slot =
                        self.current_frame().read_u8().expect("Stack slot exists") as usize;
                    let stack_start = self.current_frame().stack_start;

                    self.stack.push(
                        self.stack
                            .get(stack_start + frame_slot)
                            .expect("Local variable exists on the stack.")
                            .clone(),
                    )
                }
                OpCode::SetLocal => {
                    let frame_slot =
                        self.current_frame().read_u8().expect("Stack slot exists") as usize;
                    let stack_start = self.current_frame().stack_start;

                    let new_value = self
                        .stack
                        .last()
                        .expect("New variable value exists on the stack.")
                        .clone();
                    let variable_ref = self
                        .stack
                        .get_mut(stack_start + frame_slot)
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
                    let arg_count = self.current_frame().read_u8().expect("") as usize;

                    // Lookup the object which should be a function and handle exceptions to that
                    let Value::Object(object) = self
                        .stack
                        .get(self.stack.len() - arg_count - 1)
                        .expect("Function should exist on the stack")
                    else {
                        return Err(RuntimeError::TypeError(
                            "Expected an Object, got some other primative".to_string(),
                        ));
                    };

                    // - add a stack frame to `frames` and set ip appropriately
                    self.frames.push(
                        Frame::try_new(object.clone(), self.stack.len() - arg_count)
                            .expect("object should be a function"),
                    );
                }
            }
        }

        Ok(None)
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
            (Self::Nil, Self::Nil) => true,
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

/// Instruction signatures list bytecode operands, not values consumed from the stack.
/// Operands follow the opcode byte; `u16` offsets are encoded big-endian.
#[derive(Debug, PartialEq, Clone)]
pub enum OpCode {
    /// `Return`
    ///
    /// Pops a [Value] from the stack and unwinds a call frame or ends the script execution
    Return,
    /// `Constant <constant_index: u8>`
    ///
    /// Pushes a [Value] from the current chunk values table onto the stack
    Constant,
    /// `Negate`
    ///
    /// Pops a [Value] from the stack and pushes its negation
    Negate,
    /// `Add`
    ///
    /// Pops two [Value]'s from the stack, attempts to add them and pushes the result
    Add,
    /// `Subtract`
    ///
    /// Pops two [Value]'s from the stack, attempts to subtract them and pushes the result
    Subtract,
    /// `Multiply`
    ///
    /// Pops two [Value]'s from the stack, attempts to multiply them and pushes the result
    Multiply,
    /// `Divide`
    ///
    /// Pops two [Value]'s from the stack, attempts to divide them and pushes the result
    Divide,
    /// `Nil`
    ///
    /// Pushes a [Value::Nil] onto the stack
    Nil,
    /// `True`
    ///
    /// Pushes `Value::Bool(true)` onto the stack.
    True,
    /// `False`
    ///
    /// Pushes `Value::Bool(false)` onto the stack.
    False,
    /// `Not`
    ///
    /// Pops a [Value] from the stack, interprets the value as falsey or not and flips the result
    Not,
    /// `Equal`
    ///
    /// Pops the right operand, then the left, and pushes whether they are equal.
    Equal,
    /// `Greater`
    ///
    /// Pops the right operand, then the left, and pushes whether `left > right`.
    /// Both operands must be numbers.
    Greater,
    /// `Less`
    ///
    /// Pops the right operand, then the left, and pushes whether `left < right`.
    /// Both operands must be numbers.
    Less,
    /// `Print`
    ///
    /// Pops a [Value] and prints its display representation followed by a newline.
    Print,
    /// `DefineGlobal <name_constant_index: u8>`
    ///
    /// Pops the initial value and binds it in the VM's globals table. The indexed
    /// constant in the current chunk must be a string naming the global.
    DefineGlobal,
    /// `GetGlobal <name_constant_index: u8>`
    ///
    /// Pushes a copy of a global's value. The indexed constant in the current
    /// chunk must be a string naming an existing global.
    GetGlobal,
    /// `SetGlobal <name_constant_index: u8>`
    ///
    /// Pops a value and stores it in the VM's globals table. The indexed constant
    /// in the current chunk must be a string naming the global.
    SetGlobal,
    /// `Pop`
    ///
    /// Discards the top value from the stack.
    Pop,
    /// `GetLocal <slot: u8>`
    ///
    /// Pushes a copy of the value in the specified local slot. The current VM
    /// interprets `slot` as an absolute value-stack index.
    GetLocal,
    /// `SetLocal <slot: u8>`
    ///
    /// Copies the top value into the specified local slot without popping it.
    /// The current VM interprets `slot` as an absolute value-stack index.
    SetLocal,
    /// `JumpIfFalse <offset: u16>`
    ///
    /// Peeks at the top value and jumps forward if it is `nil` or `false`.
    /// The value remains on the stack. The byte offset is measured from the
    /// instruction pointer immediately after reading the operand.
    JumpIfFalse,
    /// `Jump <offset: u16>`
    ///
    /// Unconditionally jumps forward by `offset` bytes, measured from the
    /// instruction pointer immediately after reading the operand.
    /// Does not change the value stack.
    Jump,
    /// `Loop <offset: u16>`
    ///
    /// Unconditionally jumps backward by `offset` bytes, measured from the
    /// instruction pointer immediately after reading the operand.
    /// Does not change the value stack.
    Loop,
    /// `Call <argument_count: u8>`
    ///
    /// Starts executing a function in a new frame. Before the call, the stack
    /// must end with the function value followed by `argument_count` argument
    /// values in order. The function and arguments remain on the stack when
    /// entering the callee, and the caller's instruction pointer is preserved.
    Call,
}

impl OpCode {
    // Returns the number of arguments this opcode expects in the bytecode
    fn arity(&self) -> i32 {
        match *self {
            Self::Call => 1,
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
            x if x == OpCode::Call as u8 => Ok(OpCode::Call),
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
    /// Index in the values stack where this function's locals start
    stack_start: usize,
}

impl Frame {
    fn try_new(function: Gc<HeapObject>, slot_start: usize) -> Result<Frame, ()> {
        // validate that heap ref points to a function once here so we can just unwrap in the
        // helpers. Assumes refs don't change type
        let is_func = matches!(*function.borrow(), HeapObject::Fn(_));

        if is_func {
            Ok(Self {
                function,
                ip: 0,
                stack_start: slot_start,
            })
        } else {
            Err(())
        }
    }

    /// Read the next chunk byte and advance the IP
    fn read_u8(&mut self) -> Option<u8> {
        let byte = self.borrow_function().chunk.code.get(self.ip).copied();
        self.ip += 1;

        byte
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

    macro_rules! emit {
        (
            $chunk:expr;
            $( $op:ident $( ( $operand:expr ) )? ; )*
        ) => {{
            let chunk = &mut $chunk;

            $(
                chunk.write_op(OpCode::$op, 1);
                $(
                    chunk.write($operand, 1);
                )?
            )*
        }};
    }

    #[test]
    fn vm_executes_top_level_frame() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();
        let lhs = chunk.add_constant(Value::Number(20.0));
        let rhs = chunk.add_constant(Value::Number(22.0));
        let name = vm.heap.allocate(HeapObject::String("result".to_string()));
        let name_index = chunk.add_constant(Value::Object(name));

        emit! {
            chunk;
            Constant(lhs);
            Constant(rhs);
            Add;
            DefineGlobal(name_index);
            Nil;
            Return;
        }

        let function = vm.heap.allocate(HeapObject::Fn(Function {
            arity: 0,
            chunk,
            name: "script".to_string(),
        }));

        vm.interpret(function).expect("Top-level script should run");

        assert_eq!(vm.globals.get("result"), Some(&Value::Number(42.0)));
    }

    #[test]
    fn vm_executes_one_nested_call() {
        let mut vm = VM::new();
        let mut callee_chunk = Chunk::new();
        let returned_value = callee_chunk.add_constant(Value::Number(22.0));
        emit! {
            callee_chunk;
            Constant(returned_value);
            Return;
        }

        let callee = vm.heap.allocate(HeapObject::Fn(Function {
            arity: 0,
            chunk: callee_chunk,
            name: "twenty_two".to_string(),
        }));

        let mut script_chunk = Chunk::new();
        let callee_index = script_chunk.add_constant(Value::Object(callee));
        let addend = script_chunk.add_constant(Value::Number(20.0));
        let name = vm.heap.allocate(HeapObject::String("result".to_string()));
        let name_index = script_chunk.add_constant(Value::Object(name));

        emit! {
            script_chunk;
            Constant(callee_index);
            Call(0);
            Constant(addend);
            Add;
            DefineGlobal(name_index);
            Nil;
            Return;
        }

        let script = vm.heap.allocate(HeapObject::Fn(Function {
            arity: 0,
            chunk: script_chunk,
            name: "script".to_string(),
        }));

        vm.interpret(script).expect("Nested call should run");

        assert_eq!(vm.globals.get("result"), Some(&Value::Number(42.0)));
    }

    #[test]
    fn vm_binds_a_parameter_in_the_callee_frame() {
        let mut vm = VM::new();
        let mut callee_chunk = Chunk::new();
        // Slot zero is the first parameter, relative to this invocation's locals.
        emit! { callee_chunk; GetLocal(0); Return; }
        let callee = vm.heap.allocate(HeapObject::Fn(Function {
            arity: 1,
            chunk: callee_chunk,
            name: "identity".to_string(),
        }));

        let mut script_chunk = Chunk::new();
        let caller_local = script_chunk.add_constant(Value::Number(20.0));
        let callee_index = script_chunk.add_constant(Value::Object(callee));
        let argument = script_chunk.add_constant(Value::Number(22.0));
        emit! {
            script_chunk;
            Constant(caller_local);
            Constant(callee_index);
            Constant(argument);
            Call(1);
            GetLocal(0);
            Add;
            Return;
        }

        assert_eq!(
            run_chunk(&mut vm, script_chunk).expect("Call with a parameter should run"),
            Some(Value::Number(42.0))
        );
        assert!(
            vm.stack.is_empty(),
            "Returning must reclaim the call's stack values"
        );
        assert!(
            vm.frames.is_empty(),
            "Returning must remove all completed frames"
        );
    }

    #[test]
    fn vm_binds_a_local_in_the_callee_frame() {
        let mut vm = VM::new();
        let mut callee_chunk = Chunk::new();
        let local_value = callee_chunk.add_constant(Value::Number(22.0));
        // The initializer supplies callee-local slot zero; the copy is the return value.
        emit! {
            callee_chunk;
            Constant(local_value);
            GetLocal(0);
            Return;
        }
        let callee = vm.heap.allocate(HeapObject::Fn(Function {
            arity: 0,
            chunk: callee_chunk,
            name: "local_value".to_string(),
        }));

        let mut script_chunk = Chunk::new();
        let caller_local = script_chunk.add_constant(Value::Number(20.0));
        let callee_index = script_chunk.add_constant(Value::Object(callee));
        emit! {
            script_chunk;
            Constant(caller_local);
            Constant(callee_index);
            Call(0);
            GetLocal(0);
            Add;
            Return;
        }

        assert_eq!(
            run_chunk(&mut vm, script_chunk).expect("Call with a local should run"),
            Some(Value::Number(42.0))
        );
        assert!(
            vm.stack.is_empty(),
            "Returning must reclaim the call's stack values"
        );
        assert!(
            vm.frames.is_empty(),
            "Returning must remove all completed frames"
        );
    }

    fn run_chunk(vm: &mut VM, chunk: Chunk) -> Result<Option<Value>, RuntimeError> {
        let function = vm.heap.allocate(HeapObject::Fn(Function {
            arity: 0,
            chunk,
            name: "script".to_string(),
        }));
        vm.interpret(function)
    }

    #[test]
    fn vm_executes_numeric_binary_ops() {
        for (op, expected) in [
            (OpCode::Add, 10.0),
            (OpCode::Subtract, 6.0),
            (OpCode::Multiply, 16.0),
            (OpCode::Divide, 4.0),
        ] {
            let mut vm = VM::new();
            let mut chunk = Chunk::new();
            let lhs = chunk.add_constant(Value::Number(8.0));
            let rhs = chunk.add_constant(Value::Number(2.0));
            emit! { chunk; Constant(lhs); Constant(rhs); }
            chunk.write_op(op.clone(), 1);
            emit! { chunk; Return; }

            let result = run_chunk(&mut vm, chunk).expect("Arithmetic should succeed");
            assert_eq!(result, Some(Value::Number(expected)), "{op:?}");
        }
    }

    #[test]
    fn vm_negates_a_number() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();
        let number = chunk.add_constant(Value::Number(7.0));
        emit! { chunk; Constant(number); Negate; Return; }

        assert_eq!(
            run_chunk(&mut vm, chunk).unwrap(),
            Some(Value::Number(-7.0))
        );
    }

    #[test]
    fn vm_returns_literals() {
        for op in [OpCode::Nil, OpCode::True, OpCode::False] {
            let mut vm = VM::new();
            let mut chunk = Chunk::new();
            chunk.write_op(op.clone(), 1);
            emit! { chunk; Return; }

            let result = run_chunk(&mut vm, chunk).unwrap();
            match op {
                OpCode::Nil => assert!(matches!(result, Some(Value::Nil))),
                OpCode::True => assert!(matches!(result, Some(Value::Bool(true)))),
                OpCode::False => assert!(matches!(result, Some(Value::Bool(false)))),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn vm_not_uses_lox_truthiness() {
        for (input, expected) in [
            (Value::Nil, true),
            (Value::Bool(false), true),
            (Value::Bool(true), false),
            (Value::Number(0.0), false),
        ] {
            let mut vm = VM::new();
            let mut chunk = Chunk::new();
            let value = chunk.add_constant(input.clone());
            emit! { chunk; Constant(value); Not; Return; }

            assert_eq!(
                run_chunk(&mut vm, chunk).unwrap(),
                Some(Value::Bool(expected)),
                "Not {input:?}"
            );
        }
    }

    #[test]
    fn vm_compares_numbers_in_operand_order() {
        for (op, lhs_value, rhs_value, expected) in [
            (OpCode::Greater, 8.0, 2.0, true),
            (OpCode::Greater, 2.0, 8.0, false),
            (OpCode::Greater, 2.0, 2.0, false),
            (OpCode::Less, 2.0, 8.0, true),
            (OpCode::Less, 8.0, 2.0, false),
            (OpCode::Less, 2.0, 2.0, false),
        ] {
            let mut vm = VM::new();
            let mut chunk = Chunk::new();
            let lhs = chunk.add_constant(Value::Number(lhs_value));
            let rhs = chunk.add_constant(Value::Number(rhs_value));
            emit! { chunk; Constant(lhs); Constant(rhs); }
            chunk.write_op(op.clone(), 1);
            emit! { chunk; Return; }

            assert_eq!(
                run_chunk(&mut vm, chunk).unwrap(),
                Some(Value::Bool(expected)),
                "{lhs_value} {op:?} {rhs_value}"
            );
        }
    }

    #[test]
    fn vm_checks_equality_by_type_and_value() {
        for (lhs_value, rhs_value, expected) in [
            (Value::Number(2.0), Value::Number(2.0), true),
            (Value::Number(2.0), Value::Number(3.0), false),
            (Value::Bool(true), Value::Bool(true), true),
            (Value::Bool(true), Value::Bool(false), false),
            (Value::Nil, Value::Nil, true),
            (Value::Number(0.0), Value::Bool(false), false),
            (Value::Number(0.0), Value::Nil, false),
            (Value::Nil, Value::Number(0.0), false),
        ] {
            let mut vm = VM::new();
            let mut chunk = Chunk::new();
            let lhs = chunk.add_constant(lhs_value.clone());
            let rhs = chunk.add_constant(rhs_value.clone());
            emit! { chunk; Constant(lhs); Constant(rhs); Equal; Return; }

            assert_eq!(
                run_chunk(&mut vm, chunk).unwrap(),
                Some(Value::Bool(expected)),
                "{lhs_value:?} == {rhs_value:?}"
            );
        }
    }

    #[test]
    fn vm_add_concatenates_strings() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();
        let hello = vm.heap.allocate(HeapObject::String("hello, ".to_string()));
        let world = vm.heap.allocate(HeapObject::String("world".to_string()));
        let lhs = chunk.add_constant(Value::Object(hello));
        let rhs = chunk.add_constant(Value::Object(world));
        emit! { chunk; Constant(lhs); Constant(rhs); Add; Return; }

        let result = run_chunk(&mut vm, chunk).unwrap().unwrap();
        assert_eq!(&*result.borrow_string().unwrap(), "hello, world");
    }

    #[test]
    fn vm_rejects_invalid_numeric_operands() {
        for op in [
            OpCode::Add,
            OpCode::Subtract,
            OpCode::Multiply,
            OpCode::Divide,
            OpCode::Greater,
            OpCode::Less,
            OpCode::Negate,
        ] {
            let mut vm = VM::new();
            let mut chunk = Chunk::new();
            if op != OpCode::Negate {
                let number = chunk.add_constant(Value::Number(2.0));
                emit! { chunk; Constant(number); }
            }
            emit! { chunk; True; }
            chunk.write_op(op.clone(), 1);
            emit! { chunk; Return; }

            assert!(
                matches!(run_chunk(&mut vm, chunk), Err(RuntimeError::TypeError(_))),
                "{op:?} should reject a boolean operand"
            );
        }
    }

    #[test]
    fn vm_pop_discards_only_the_top_value() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();
        let number = chunk.add_constant(Value::Number(42.0));
        emit! { chunk; Constant(number); True; Pop; Return; }

        assert_eq!(
            run_chunk(&mut vm, chunk).unwrap(),
            Some(Value::Number(42.0))
        );
    }

    #[test]
    fn vm_gets_and_sets_a_local() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();
        let initial = chunk.add_constant(Value::Number(10.0));
        let replacement = chunk.add_constant(Value::Number(42.0));
        emit! {
            chunk;
            Constant(initial);
            Constant(replacement);
            SetLocal(0);
            Pop;
            GetLocal(0);
            Return;
        }

        assert_eq!(
            run_chunk(&mut vm, chunk).unwrap(),
            Some(Value::Number(42.0))
        );
    }

    #[test]
    fn vm_defines_updates_and_reads_a_global() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();
        let name = vm.heap.allocate(HeapObject::String("answer".to_string()));
        let name_index = chunk.add_constant(Value::Object(name));
        let initial = chunk.add_constant(Value::Number(10.0));
        let replacement = chunk.add_constant(Value::Number(42.0));
        emit! {
            chunk;
            Constant(initial);
            DefineGlobal(name_index);
            Constant(replacement);
            SetGlobal(name_index);
            GetGlobal(name_index);
            Return;
        }

        assert_eq!(
            run_chunk(&mut vm, chunk).unwrap(),
            Some(Value::Number(42.0))
        );
        assert_eq!(vm.globals.get("answer"), Some(&Value::Number(42.0)));
    }

    #[test]
    fn vm_jump_reads_a_two_byte_offset() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();
        let result = chunk.add_constant(Value::Number(42.0));
        // The offset 0x0100 skips 256 one-byte instructions.
        emit! { chunk; Jump(1); }
        chunk.write(0, 1);
        for _ in 0..256 {
            emit! { chunk; Nil; }
        }
        emit! { chunk; Constant(result); Return; }

        assert_eq!(
            run_chunk(&mut vm, chunk).unwrap(),
            Some(Value::Number(42.0))
        );
        assert!(vm.stack.is_empty(), "Skipped instructions must not execute");
    }

    #[test]
    fn vm_jump_if_false_selects_the_correct_branch() {
        for (condition, expected) in [(false, 20.0), (true, 10.0)] {
            let mut vm = VM::new();
            let mut chunk = Chunk::new();
            let condition_index = chunk.add_constant(Value::Bool(condition));
            let then_value = chunk.add_constant(Value::Number(10.0));
            let else_value = chunk.add_constant(Value::Number(20.0));
            emit! { chunk; Constant(condition_index); JumpIfFalse(0); }
            chunk.write(6, 1); // Skip the then branch, including its unconditional jump.
            emit! { chunk; Pop; Constant(then_value); Jump(0); }
            chunk.write(3, 1); // Skip the else branch.
            emit! { chunk; Pop; Constant(else_value); Return; }

            assert_eq!(
                run_chunk(&mut vm, chunk).unwrap(),
                Some(Value::Number(expected)),
                "condition = {condition}"
            );
        }
    }

    #[test]
    fn vm_loop_counts_down_a_local() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();
        let three = chunk.add_constant(Value::Number(3.0));
        let zero = chunk.add_constant(Value::Number(0.0));
        let one = chunk.add_constant(Value::Number(1.0));
        emit! {
            chunk;
            Constant(three);
            GetLocal(0);
            Constant(zero);
            Greater;
            JumpIfFalse(0);
        }
        chunk.write(12, 1); // Exit at the condition cleanup after the loop body.
        emit! {
            chunk;
            Pop;
            GetLocal(0);
            Constant(one);
            Subtract;
            SetLocal(0);
            Pop;
            Loop(0);
        }
        chunk.write(20, 1); // Return to GetLocal at byte offset 2.
        emit! { chunk; Pop; GetLocal(0); Return; }

        assert_eq!(run_chunk(&mut vm, chunk).unwrap(), Some(Value::Number(0.0)));
    }

    #[test]
    fn vm_reports_a_chunk_without_return() {
        let mut vm = VM::new();

        assert!(matches!(
            run_chunk(&mut vm, Chunk::new()),
            Err(RuntimeError::NoReturn)
        ));
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
