use crate::chunk::Chunk;
use crate::op::OpCode;

#[derive(Debug)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

#[derive(Debug)]
pub struct VM {
    //chunk: &'a Chunk,
}

impl VM {
    pub fn interpret(&mut self, chunk: &Chunk) -> InterpretResult {
        for op in chunk.code.iter() {
            match op {
                OpCode::Return => return InterpretResult::Ok,
                OpCode::Constant { index } => {
                    let constant = chunk.read_constant(*index);
                    println!("{constant}")
                }
            }
        }

        InterpretResult::RuntimeError
    }
}
