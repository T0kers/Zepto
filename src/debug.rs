use crate::{chunk::{Chunk, OpCode}, value::Value};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("==== {} ====", name);
    let mut i: usize = 0;
    while i < chunk.code.len() {
        i = disassemble_instruction(chunk, i);
    }
    println!("{}", String::from_utf8(vec![b'='; name.len() + 10]).unwrap());
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
        OpCode::TRUE => simple_instruction("TRUE", i),
        OpCode::FALSE => simple_instruction("FALSE", i),
        OpCode::NUL => simple_instruction("NUL", i),
        OpCode::ADD => simple_instruction("ADD", i),
        OpCode::SUB => simple_instruction("SUB", i),
        OpCode::MUL => simple_instruction("MUL", i),
        OpCode::DIV => simple_instruction("DIV", i),
        OpCode::REM => simple_instruction("REM", i),
        OpCode::UMINUS => simple_instruction("UMINUS", i),
        OpCode::BITOR => simple_instruction("BITOR", i),
        OpCode::BITAND => simple_instruction("BITAND", i),
        OpCode::BITXOR => simple_instruction("BITXOR", i),
        // OpCode::BITNOT => simple_instruction("BITNOT", i),
        // OpCode::BITSHL => simple_instruction("BITSHL", i),
        // OpCode::BITSHR => simple_instruction("BITSHR", i),
        OpCode::EQUAL => simple_instruction("EQUAL", i),
        OpCode::LESS => simple_instruction("LESS", i),
        OpCode::DEFINE_GLOBAL => variable_instruction("DEFINE_GLOBAL", chunk, i),
        OpCode::DEFINE_GLOBAL_LONG => variable_instruction_long("DEFINE_GLOBAL_LONG", chunk, i),
        OpCode::SET_GLOBAL => variable_instruction("SET_GLOBAL", chunk, i),
        OpCode::SET_GLOBAL_LONG => variable_instruction_long("SET_GLOBAL_LONG", chunk, i),
        OpCode::GET_GLOBAL => variable_instruction("GET_GLOBAL", chunk, i),
        OpCode::GET_GLOBAL_LONG => variable_instruction_long("GET_GLOBAL_LONG", chunk, i),
        OpCode::GREATER => simple_instruction("GREATER", i),
        OpCode::NOT => simple_instruction("NOT", i),
        OpCode::PRINT => simple_instruction("PRINT", i),
        OpCode::POP => simple_instruction("POP", i),
        OpCode::RETURN => simple_instruction("RETURN", i),
        OpCode::EOF => simple_instruction("EOF", i),
        _ => {println!("????"); i + 1},
    }
}

fn variable_instruction(name: &str, chunk: &Chunk, i: usize) -> usize {
    let index = chunk.code[i + 1] as usize;
    println!("{} {}", name, index);
    i + 2
}

fn variable_instruction_long(name: &str, chunk: &Chunk, i: usize) -> usize {
    let index: usize = ((chunk.code[i + 1] as usize) << 8) | (chunk.code[i + 2] as usize);
    println!("{} {}", name, index);
    i + 3
}

fn constant_instruction(name: &str, chunk: &Chunk, i: usize) -> usize {
    let index = chunk.code[i + 1] as usize;
    let constant = match &chunk.constants[index] {
        Value::Obj(o) => o.debug_string(),
        other => other.to_string(),
    };
    println!("{} {} {}", name, index, constant);
    i + 2
}

fn constant_long_instruction(name: &str, chunk: &Chunk, i: usize) -> usize {
    let index: usize = ((chunk.code[i + 1] as usize) << 8) | (chunk.code[i + 2] as usize);
    let constant = match &chunk.constants[index] {
        Value::Obj(o) => o.debug_string(),
        other => other.to_string(),
    };
    println!("{} {} {}", name, index, constant);
    i + 3
}

fn simple_instruction(name: &str, i: usize) -> usize {
    println!("{}", name);
    i + 1
}