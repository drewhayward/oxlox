use oxlox::{chunk::Chunk, op::OpCode, vm::VM};

fn main() {
    let mut chunk = Chunk::new();

    let index = chunk.add_constant(42.0);
    chunk.write(OpCode::Constant { index }, 1);
    chunk.write(OpCode::Return, 2);
    chunk.disassemble("hello");

    let mut vm = VM{};

    let result = vm.interpret(&chunk);

    println!("{:?}", result)

}
