use std::io::{self, Write};

use crate::{chunk::{Chunk, OpCode}, debug::disassemble_chunk, scanner::Scanner, scanner::Token, scanner::TokenKind, value::Value, vm::VMError};

pub struct Compiler<'a> {
    current: Token,
    previous: Token,
    scanner: Scanner<'a>,
    pub chunk: Chunk,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            current: Token::new(TokenKind::Nul, 0, 0, 0),
            previous: Token::new(TokenKind::Nul, 0, 0, 0),
            scanner: Scanner::new(source),
            chunk: Chunk::new(),
            had_error: false,
            panic_mode: false,
        }
    }
    pub fn compile(&mut self) -> Result<(), VMError> {
        self.advance();
        
        self.expression();

        self.consume(TokenKind::EOF, "Expected end of expression.");
        self.emit_byte(OpCode::RETURN);
        self.emit_byte(OpCode::EOF);

        if self.had_error {
            Err(VMError::CompileError)
        }
        else {
            #[cfg(feature = "print_code")]
            {
                disassemble_chunk(self.current_chunk(), "bytecode");
            }
            Ok(())
        }
    }
    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
}

// parsing
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    None,
    Assignment,  // =
    Or,          // ||
    And,         // &&
    Equality,    // == !=
    Comparison,  // < > <= >=
    BitwiseOr,   // |
    BitwiseXor,  // ^
    BitwiseAnd,  // &
    Term,        // + -
    Factor,      // * /
    Unary,       // ! - ~
    Call,        // . ()
    Primary
}

impl Precedence {
    pub fn one_higher(&self) -> Self {
        use Precedence as P;
        match *self {
            P::None => P::Assignment,
            P::Assignment => P::Or,
            P::Or => P::And,
            P::And => P::Equality,
            P::Equality => P::Comparison,
            P::Comparison => P::BitwiseOr,
            P::BitwiseOr => P::BitwiseXor,
            P::BitwiseXor => P::BitwiseAnd,
            P::BitwiseAnd => P::Term,
            P::Term => P::Factor,
            P::Factor => P::Unary,
            P::Unary => P::Call,
            P::Call => P::Primary,
            P::Primary => P::Primary,
        }
    }
}

impl<'a> Compiler<'a> {
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
    fn consume(&mut self, kind: TokenKind, msg: &str) {
        if self.current.kind == kind {
            self.advance();
        }
        else {
            self.error_at_current(msg);
        }
    }

    fn parse_precedence(&mut self, prec: Precedence) {
        self.advance();
        if let Some(prefix_rule) = self.get_rule(&self.previous.kind).prefix {
            prefix_rule(self);
        }
        else {
            self.error("Expected expression.");
            return;
        }

        while prec <= self.get_rule(&self.current.kind).precedence {
            self.advance();
            if let Some(infix_rule) = self.get_rule(&self.previous.kind).infix {
                infix_rule(self);
            }

        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }
    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenKind::RParen, "Expected ')' after expression.");
    }
    fn unary(&mut self) {
        let oper_kind = self.previous.kind.clone();
        self.parse_precedence(Precedence::Unary);
        match oper_kind {
            TokenKind::Minus => {self.emit_byte(OpCode::NEG)},
            TokenKind::Tilde => {},
            TokenKind::Exclamation => {},
            _ => {}, // unreachable
        }
    }
    fn binary(&mut self) {
        let oper_kind = self.previous.kind.clone();
        let rule = self.get_rule(&oper_kind);
        self.parse_precedence(rule.precedence.one_higher());
        
        match oper_kind {
            TokenKind::Plus => {self.emit_byte(OpCode::ADD)},
            TokenKind::Minus => {self.emit_byte(OpCode::SUB)},
            TokenKind::Star => {self.emit_byte(OpCode::MUL)},
            TokenKind::Slash => {self.emit_byte(OpCode::DIV)},
            TokenKind::StarStar => {},
            TokenKind::ExclamationEqual => {},
            TokenKind::EqualEqual => {},
            TokenKind::Greater => {},
            TokenKind::GreaterEqual => {},
            TokenKind::GreaterGreater => {},
            TokenKind::Less => {},
            TokenKind::LessEqual => {},
            TokenKind::LessLess => {},
            TokenKind::And => {},
            TokenKind::AndAnd => {},
            TokenKind::Bar => {},
            TokenKind::BarBar => {},
            TokenKind::Carrot => {},
            TokenKind::CarrotCarrot => {},
            _ => {}, // unreachable
        }
    }
}

// emitting bytes
impl<'a> Compiler<'a> {
    fn emit_byte(&mut self, byte: u8) {
        let prev_line = self.previous.line;
        self.current_chunk().add_opcode(byte, prev_line);
    }
    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }
    fn emit_number(&mut self) {
        let value: i64 = self.previous.lexeme(self.scanner.source).parse().unwrap();
        self.emit_constant(Value::Int(value));
    }
    fn emit_literal(&mut self) {
        self.emit_byte(match self.previous.kind {
            TokenKind::True => OpCode::TRUE,
            TokenKind::False => OpCode::FALSE,
            TokenKind::Nul => OpCode::NUL,
            _ => unreachable!(),
        })
    }
    fn emit_constant(&mut self, value: Value) {
        let prev_line = self.previous.line;
        if self.current_chunk().write_constant(value, prev_line).is_err() {
            self.error("Too many constants in one chunk.");
        }
    }
}


struct ParseRule {
    prefix: Option<fn(&mut Compiler)>,
    infix: Option<fn(&mut Compiler)>,
    precedence: Precedence,
}

impl ParseRule {
    pub const fn new(prefix: Option<fn(&mut Compiler)>, infix: Option<fn(&mut Compiler)>, precedence: Precedence) -> Self {
        Self {prefix, infix, precedence}
    } 
}

impl<'a> Compiler<'a> {
    fn get_rule(&self, kind: &TokenKind) -> &'static ParseRule {
        use TokenKind as TK;
        static LPAREN_RULE: ParseRule = ParseRule::new(Some(|s| s.grouping()), None, Precedence::None);
        static PLUS_RULE: ParseRule = ParseRule::new(None, Some(|s| s.binary()), Precedence::Term);
        static MINUS_RULE: ParseRule = ParseRule::new(Some(|s| s.unary()), Some(|s| s.binary()), Precedence::Term);
        static TERM_RULE: ParseRule = ParseRule::new(None, Some(|s| s.binary()), Precedence::Factor);
        static INT_RULE: ParseRule = ParseRule::new(Some(|s| s.emit_number()), None, Precedence::None);
        static LITERAL_RULE: ParseRule = ParseRule::new(Some(|s| s.emit_literal()), None, Precedence::None);
        static DEFAULT_RULE: ParseRule = ParseRule::new(None, None, Precedence::None);

        match kind {
            TK::LParen => &LPAREN_RULE,
            TK::Plus => &PLUS_RULE,
            TK::Minus => &MINUS_RULE,
            TK::Star | TK::Slash => &TERM_RULE,
            TK::Int => &INT_RULE,
            TK::True | TK::False | TK::Nul => &LITERAL_RULE,
            _ => &DEFAULT_RULE,
        }
    }
}

// error handling
impl<'a> Compiler<'a> {
    fn error_at_current(&mut self, msg: &str) {
        self.error_at(self.current.clone(), msg);
    }
    fn error(&mut self, msg: &str) {
        self.error_at(self.previous.clone(), msg);
    }
    fn error_at(&mut self, token: Token, msg: &str) {
        if self.panic_mode {return;}
        self.panic_mode = true;
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

