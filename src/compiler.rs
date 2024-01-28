use crate::{scanner::Scanner, scanner::TokenKind, vm::VMError, chunk::Chunk};

pub fn compile(source: &str) -> Result<Chunk, VMError> {
    let mut scanner = Scanner::new(source);
    let mut line: u32 = 0;
    loop {
        let token = scanner.scan_token();
        if token.line != line {
            line = token.line;
            print!("{:4} ", line);
        }
        else {
            print!("   | ");
        }
        println!("{:?} {}", token.kind, source[token.start..token.end].to_string());
        if token.kind == TokenKind::EOF {
            break;
        }
    }
    Err(VMError::CompileError)
}