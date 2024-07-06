use oxlox;
use std::{fs, io::Read};

fn help() {
    println!(
        "Usage:
oxlox <lox_file>
"
    )
}

fn run(args: Vec<String>) {
    let filename = args.get(1).expect("Filename expected.");

    let mut file = fs::File::open(filename).expect("Failed to open lox file.");

    let mut source: String = String::new();
    let _  = file.read_to_string(&mut source).expect("Failed to read file.");

    let scanner = oxlox::scanner::Scanner::new(&source);
    let tokens: Vec<_> = scanner.collect();

    println!("{:?}", tokens);

    let mut compiler = oxlox::compiler::Compiler::new(tokens);
    let code_chunk = compiler.compile();

    code_chunk.disassemble("Complied");

    let mut vm = oxlox::vm::VM::new();
    let _ = vm.interpret(&code_chunk);
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
