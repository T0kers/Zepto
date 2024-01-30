use std::io::{self, Write};

use crate::{scanner::Scanner, scanner::TokenKind, scanner::Token, vm::VMError, chunk::Chunk};

pub struct Compiler<'a> {
    current: Token,
    previous: Token,
    scanner: Scanner<'a>,
    pub chunk: Chunk,
    had_error: bool
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            current: Token::new(TokenKind::Nul, 0, 0, 0),
            previous: Token::new(TokenKind::Nul, 0, 0, 0),
            scanner: Scanner::new(source),
            chunk: Chunk::new(),
            had_error: false,
        }
    }
    pub fn compile(&mut self) -> Result<(), VMError> {
        loop {
            let token = self.scanner.scan_token();
            
            if token.kind == TokenKind::EOF {
                break;
            }
        }
        if self.had_error {
            Err(VMError::CompileError)
        }
        else {
            Ok(())
        }
    }
    fn advance(&mut self) {
        self.previous = self.current.clone();
        loop {
            self.current = self.scanner.scan_token();
            if let TokenKind::Error(e) = self.current.kind.clone() {
                self.error_at_current(&e);
            }
            else {
                break;
            }
        }
    }
    fn error_at_current(&mut self, msg: &str) {
        self.error_at(self.current.clone(), msg);
    }
    fn error(&mut self, msg: &str) {
        self.error_at(self.previous.clone(), msg);
    }
    fn error_at(&mut self, token: Token, msg: &str) {
        eprint!("[Line {}] Error", token.line);
        io::stdout().flush().unwrap();
        match token.kind {
            TokenKind::EOF => eprint!(" at end"),
            _ => eprint!(" at {}", token.lexeme(self.scanner.source))
        }
        eprintln!(": {}", msg);
        self.had_error = true;
    }
}


