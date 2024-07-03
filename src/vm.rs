use crate::value::Value;

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


#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<OpCode>,
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

    pub fn write(&mut self, op: OpCode, line_num: u32) {
        self.code.push(op);
        self.line_nums.push(line_num);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }

    pub fn read_constant(&self, index: usize) -> Value {
        return self.values[index]
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        for (i, (op, line)) in self.code.iter().zip(self.line_nums.iter()).enumerate() {
            println!("{:04} {} {:?}", i, line, op);
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    CompileError,
    RuntimeError,
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

    pub fn interpret(&mut self, chunk: &Chunk) -> Result<(), ErrorKind> {
        for op in chunk.code.iter() {
            // Consider a debug output of op codes and stack state here as we go
            match op {
                OpCode::Return => {
                    let ret = self.stack.pop().expect("Should have a value to pop");
                    println!("{ret}");
                    return Ok(())
                }
                OpCode::Constant { index } => {
                    let constant = chunk.read_constant(*index);
                    self.stack.push(constant)
                }
                OpCode::Negate => {
                    let value = self.stack.pop().expect("Stack should have a value.");
                    self.stack.push(-value);
                }
                OpCode::Add => {
                    let rhs = self.stack.pop().expect("Should have an RHS value to add");
                    let lhs = self.stack.pop().expect("Should have an LHS value to add");
                    self.stack.push(lhs + rhs);
                }
                OpCode::Subtract => {
                    let rhs = self.stack.pop().expect("Should have an RHS value to subtract");
                    let lhs = self.stack.pop().expect("Should have an LHS value to subtract");
                    self.stack.push(lhs - rhs);
                }
                OpCode::Multiply => {
                    let rhs = self.stack.pop().expect("Should have an RHS value to multiply");
                    let lhs = self.stack.pop().expect("Should have an LHS value to multiply");
                    self.stack.push(lhs * rhs);
                }
                OpCode::Divide => {
                    let rhs = self.stack.pop().expect("Should have an RHS value to divide");
                    let lhs = self.stack.pop().expect("Should have an LHS value to divide");
                    self.stack.push(lhs / rhs);
                }
            }
        }

        Err(ErrorKind::RuntimeError)
    }

    pub fn reset(&mut self) {
        self.stack.clear()
    }
}
