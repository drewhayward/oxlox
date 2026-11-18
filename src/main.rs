use oxlox::{self, compiler::compile, object::HeapObject};
use std::{fs, io::Read};

fn help() {
    println!(
        "Usage:
oxlox <lox_file>
"
    )
}

fn run(args: Vec<String>) {
    let mut vm = oxlox::vm::VM::new();
    let filename = args.get(1).expect("Filename expected.");

    let mut file = fs::File::open(filename).expect("Failed to open lox file.");

    let mut source: String = String::new();
    let _  = file.read_to_string(&mut source).expect("Failed to read file.");

    println!("== Source: {filename} ==\n{source}");

    let script = compile(source, &mut vm.heap).expect("Script should compile");

    {
        let script_object = script.borrow();
        let HeapObject::Fn(function) = &*script_object else {
            unreachable!("Compiler should return a function object");
        };
        function.chunk.disassemble(&function.name);
    }

    let _ = vm.interpret(script).expect("Ran into a problem");
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.len() {
        1 => help(),
        2 => run(args),
        _ => {
            println!("Too many arguments!");
            help();
        }
    }
}
