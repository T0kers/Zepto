

use crate::chunk::*;
use crate::value::*;

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    let mut i: usize = 0;
    while i < chunk.code.len() {
        i = disassemble_instruction(chunk, i);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, i: usize) -> usize {
    print!("{:04} ", i);
    if i > 0 && chunk.line(i) == chunk.line(i - 1) {
        print!("   | ");
    }
    else {
        print!("{:4} ", chunk.line(i));
    }
    match chunk.code[i] {
        OpCode::CONSTANT => constant_instruction("CONSTANT", chunk, i),
        OpCode::CONSTANT_LONG => constant_long_instruction("CONSTANT_LONG", chunk, i),
        OpCode::RETURN => simple_instruction("RETURN", i),
        OpCode::EOF => simple_instruction("EOF", i),
        _ => {println!("????"); i + 1},
    }
}

fn constant_instruction(name: &str, chunk: &Chunk, i: usize) -> usize {
    let constant = chunk.code[i + 1];
    println!("{} {} {}", name, constant, value_to_string(chunk.constants[constant as usize]));
    i + 2
}

fn constant_long_instruction(name: &str, chunk: &Chunk, i: usize) -> usize {
    let constant: usize = ((chunk.code[i + 1] as usize) << 8) | (chunk.code[i + 2] as usize);
    println!("{} {} {}", name, constant, value_to_string(chunk.constants[constant]));
    i + 3
}

fn simple_instruction(name: &str, i: usize) -> usize {
    println!("{}", name);
    i + 1
}