use oxlox::vm::{Chunk, OpCode, VM};

fn main() {
    let mut chunk = Chunk::new();

    let index = chunk.add_constant(42.0);
    chunk.write(OpCode::Constant { index }, 1);
    let index = chunk.add_constant(43.0);
    chunk.write(OpCode::Constant { index }, 1);
    chunk.write(OpCode::Subtract, 1);
    chunk.write(OpCode::Return, 2);
    chunk.disassemble("hello");

    let mut vm = VM::new();

    let result = vm.interpret(&chunk);

    println!("{:?}", result)

}
