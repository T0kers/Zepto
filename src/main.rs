#![allow(clippy::missing_safety_doc)]

pub mod chunk;
pub mod debug;
pub mod value;
pub mod vm;
pub mod interpreter;
pub mod compiler;
pub mod scanner;
pub mod object;
pub mod errors;

use value::Value;
use std::{env, fs, io::{self, Write}};

fn main() {
    env::set_var("RUST_BACKTRACE", "1");
    println!("Size of Value: {}", ::std::mem::size_of::<Value>());
    // println!("Size of Object: {}", ::std::mem::size_of::<object::Object>());
    println!("Size of Option value: {}", ::std::mem::size_of::<Option<Value>>());
    println!("Size of native function: {}", ::std::mem::size_of::<fn([Value])>());
    
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
            Err(e) => eprintln!("{}", e)
        }
    }
}

fn run_file(path: &str) {
    let source = match fs::read_to_string(path) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("Unable to read file: {}", e);
            return;
        }
    };

    match interpreter::interpret(&source) {
        Ok(v) => println!("Program exited succesfully with value: {}", v),
        Err(e) => eprintln!("{}", e),
    }
}