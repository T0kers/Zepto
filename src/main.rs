pub mod chunk;
pub mod debug;
pub mod value;
pub mod vm;
pub mod interpreter;
pub mod compiler;
pub mod scanner;

use vm::VMError;

use std::env;
use std::fs;
use std::io;
use std::io::Write;

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => repl(),
        2 => run_file(&args[1]),
        _ => eprintln!("Too many arguments provided.")
    }


    // let val = Value::Int(2);
    // println!("{}", std::mem::size_of_val(&val));

    // let mut vm: VM = VM::new();
    
    // vm.chunk.write_constant(Value::Int(3), 1);
    // vm.chunk.write_constant(Value::Int(4), 1);
    // vm.chunk.write_constant(Value::Int(5), 1);
    // vm.chunk.add_opcode(OpCode::ADD, 2);
    // vm.chunk.add_opcode(OpCode::MUL, 2);
    // vm.chunk.add_opcode(OpCode::RETURN, 2);
    // vm.chunk.add_opcode(OpCode::EOF, 3);
    // debug::disassemble_chunk(&vm.chunk, "Test chunk");
    
}



fn repl() {
    let mut line = String::new();
    loop {
        line.clear();
        print!(">");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut line).expect("Something went wrong when reading input.");
        if line.is_empty() {
            break;
        }
        match interpreter::interpret(&line) {
            Ok(_) => {},
            Err(e) => match e {
                VMError::CompileError => println!("Error when compiling!"),
                VMError::RuntimeError => println!("Runtime error ocurred!"),
            }
        }
    }
}

fn run_file(path: &str) {
    let source = fs::read_to_string(path).expect("Unable to read file.");

    match interpreter::interpret(&source) {
        Ok(_) => println!("Program exited succesfully!"),
        Err(e) => match e {
            VMError::CompileError => println!("Error when compiling!"),
            VMError::RuntimeError => println!("Runtime error ocurred!"),
        }
    }
}