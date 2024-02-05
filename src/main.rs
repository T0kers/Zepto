#![allow(clippy::missing_safety_doc)]

pub mod chunk;
pub mod debug;
pub mod value;
pub mod vm;
pub mod interpreter;
pub mod compiler;
pub mod scanner;
pub mod object;

use vm::VMError;
use value::Value;
use std::{env, fs, io::{self, Write}};

fn main() {
    env::set_var("RUST_BACKTRACE", "1");
    println!("Size of Value: {}", ::std::mem::size_of::<Value>());
    println!("Size of Object: {}", ::std::mem::size_of::<object::Object>());
    
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => repl(),
        2 => run_file(&args[1]),
        _ => eprintln!("Too many arguments provided.")
    }    
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