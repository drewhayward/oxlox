#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Nil,
    Number(f64),
    Bool(bool),
}

impl Value {
    fn equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs == rhs,
            (Self::Nil, _) => true,
            (Self::Number(lhs), Self::Number(rhs)) => lhs == rhs,
            _ => false,
        }
    }

    fn is_falsey(&self) -> bool {
        match self {
            Self::Nil | Self::Bool(false) => true,
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
}

impl OpCode {
    // Returns the number of arguments this opcode expects in the bytecode
    fn arity(&self) -> i32 {
        match *self {
            Self::Constant => 1,
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
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub values: Vec<Value>,
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

#[derive(Debug)]
pub struct VM {
    pub debug: bool,
    stack: Vec<Value>,
}

impl VM {
    pub fn new() -> VM {
        VM {
            stack: Vec::new(),
            debug: false,
        }
    }

    pub fn interpret(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
        let mut ip = chunk.code.iter();
        while let Some(current_byte) = ip.next() {
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
                    let index = *ip.next().expect("Expected index following constant op.");
                    let constant = chunk.read_constant(index);
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
                    let rhs = self.stack.pop().expect("Should have an RHS value to add");
                    let lhs = self.stack.pop().expect("Should have an LHS value to add");

                    match (lhs, rhs) {
                        (Value::Number(lhs_value), Value::Number(rhs_value)) => {
                            self.stack.push(Value::Number(lhs_value + rhs_value))
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
                    self.stack.push(Value::Bool(lhs.equal(&rhs)));
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
            }
        }

        Err(RuntimeError::NoReturn)
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
