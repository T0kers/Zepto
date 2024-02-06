use std::{io::{self, Write}, collections::HashMap};

use crate::{chunk::{Chunk, OpCode}, scanner::{Scanner, Token, TokenKind}, value::{Value, Number}, vm::VMError, object::Object};

pub struct Globals {
    global_ids: HashMap<String, u16>,
    pub next_id: u16,
}

impl Globals {
    pub fn new() -> Self {
        Self {
            global_ids: HashMap::new(),
            next_id: 0,
        }
    }
    pub fn create_or_get_id(&mut self, name: &str) -> u16 {
        if let Some(id) = self.global_ids.get(name) {
            *id
        }
        else {
            let id = self.next_id;
            self.global_ids.insert(name.to_string(), id);
            self.next_id += 1;
            id
        }
    }
    pub fn find_id(&self, name: &str) -> Option<u16> {
        self.global_ids.get(name).cloned()
    }
}

impl Default for Globals {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Compiler<'a> {
    current: Token,
    previous: Token,
    scanner: Scanner<'a>,
    pub chunk: Chunk<'a>,
    pub globals: Globals,
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
            globals: Globals::new(),
            had_error: false,
            panic_mode: false,
        }
    }
    pub fn compile(&mut self) -> Result<(), VMError> {
        self.advance();
        
        while !self.compare(TokenKind::EOF) {
            self.statement();
        }

        self.consume(TokenKind::EOF, "Expected end of tokens.");
        self.emit_byte(OpCode::EOF);

        if self.had_error {
            Err(VMError::CompileError)
        }
        else {
            #[cfg(feature = "print_code")]
            {
                use crate::debug;
                debug::disassemble_chunk(self.current_chunk(), "bytecode");
            }
            Ok(())
        }
    }
    fn current_chunk(&mut self) -> &mut Chunk<'a> {
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
    Factor,      // * / %
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
    fn compare(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        }
        else {
            false
        }
    }
    fn check(&mut self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    fn parse_precedence(&mut self, prec: Precedence) {
        self.advance();

        let can_assign = prec <= Precedence::Assignment;
        if let Some(prefix_rule) = self.get_rule(&self.previous.kind).prefix {
            prefix_rule(self, can_assign);
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
        if can_assign && self.compare(TokenKind::Equal) {
            self.error("Invalid assignment target.");
        }
    }
    fn statement(&mut self) {
        match self.current.kind.clone() {
            TokenKind::Print => self.print_statement(),
            TokenKind::Let => self.let_statement(),
            // TokenKind::Fn => self.fn_statement(),
            // TokenKind::Return => self.return_statement(),
            // TokenKind::If => self.if_statement(),
            // TokenKind::While => self.while_statement(),
            // TokenKind::For => self.for_statement(),
            // TokenKind::Break => self.break_statement(),
            // TokenKind::Continue => self.continue_statement(),
            _ => self.expression_statement(),
        }

        if self.panic_mode {
            self.syncronize();
        }
    }
    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenKind::Semicolon, "Expected ';' after expression.");
        self.emit_byte(OpCode::POP);
    }
    fn print_statement(&mut self) {
        self.advance();
        self.expression();
        self.consume(TokenKind::Semicolon, "Expected ';' at end of printing value.");
        self.emit_byte(OpCode::PRINT);
    }
    fn let_statement(&mut self) {
        self.advance();
        let id = self.parse_global_variable("Expected variable name.");
        if self.compare(TokenKind::Equal) {
            self.expression()
        }
        else {
            self.emit_byte(OpCode::NUL);
        }
        self.consume(TokenKind::Semicolon, "Expected ';' after variable declaration.");
        self.emit_global_definition(id);

    }
    pub fn parse_global_variable(&mut self, msg: &str) -> u16 {
        self.consume(TokenKind::Identifier, msg);
        self.globals.create_or_get_id(self.previous.lexeme(self.scanner.source))
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
            TokenKind::Minus => {self.emit_byte(OpCode::UMINUS)},
            TokenKind::Tilde => {},
            TokenKind::Exclamation => {self.emit_byte(OpCode::NOT)},
            _ => unreachable!()
        }
    }
    fn binary(&mut self) {
        let oper_kind = self.previous.kind.clone();
        let rule = self.get_rule(&oper_kind);
        self.parse_precedence(rule.precedence.one_higher());
        
        match oper_kind {
            TokenKind::Plus => self.emit_byte(OpCode::ADD),
            TokenKind::Minus => self.emit_byte(OpCode::SUB),
            TokenKind::Star => self.emit_byte(OpCode::MUL),
            TokenKind::Slash => self.emit_byte(OpCode::DIV),
            TokenKind::Rem => self.emit_byte(OpCode::REM),
            TokenKind::StarStar => {},
            TokenKind::ExclamationEqual => self.emit_bytes(OpCode::EQUAL, OpCode::NOT), // TODO: maybe create custom instruction
            TokenKind::EqualEqual => self.emit_byte(OpCode::EQUAL),
            TokenKind::Greater => self.emit_byte(OpCode::GREATER),
            TokenKind::GreaterEqual => self.emit_bytes(OpCode::LESS, OpCode::NOT),  // TODO: maybe create custom instruction
            TokenKind::GreaterGreater => {},
            TokenKind::Less => self.emit_byte(OpCode::LESS),
            TokenKind::LessEqual => self.emit_bytes(OpCode::GREATER, OpCode::NOT),  // TODO: maybe create custom instruction
            TokenKind::LessLess => {},
            TokenKind::And => self.emit_byte(OpCode::BITAND),
            TokenKind::AndAnd => {},
            TokenKind::Bar => self.emit_byte(OpCode::BITOR),
            TokenKind::BarBar => {},
            TokenKind::Carrot => self.emit_byte(OpCode::BITXOR),
            TokenKind::CarrotCarrot => {},
            _ => unreachable!()
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
        match self.previous.kind {
            TokenKind::Int => {
                let value: i64 = self.previous.lexeme(self.scanner.source).parse().unwrap();
                self.emit_constant(Value::Num(Number::Int(value)));
            },
            TokenKind::Float => {
                let value: f64 = self.previous.lexeme(self.scanner.source).parse().unwrap();
                self.emit_constant(Value::Num(Number::Float(value)));
            },
            _ => unreachable!(),
        }
    }
    fn emit_literal(&mut self) {
        self.emit_byte(match self.previous.kind {
            TokenKind::True => OpCode::TRUE,
            TokenKind::False => OpCode::FALSE,
            TokenKind::Nul => OpCode::NUL,
            _ => unreachable!(),
        })
    }
    fn emit_string(&mut self) {
        self.emit_constant(Value::Obj(Box::new(Object::Str(
            self.previous.lexeme(self.scanner.source)[1..self.previous.lexeme(self.scanner.source).len() - 1].to_string() // TODO: make the amazing æ, ø and å work PLEASE!!!!!!!
        ))));
    }
    fn emit_global_definition(&mut self, id: u16) {
        if id > (u8::MAX as u16) {
            self.emit_byte(OpCode::DEFINE_GLOBAL_LONG);
            self.emit_bytes((id >> 8) as u8, (id & 0xFF) as u8);
        }
        else {
            self.emit_bytes(OpCode::DEFINE_GLOBAL, id as u8);
        }
    }
    fn emit_variable(&mut self, can_assign: bool) {
        let id = self.globals.create_or_get_id(self.previous.lexeme(self.scanner.source));

        let (instruction, instruction_long) = if can_assign && self.compare(TokenKind::Equal) {
            self.expression();
            (OpCode::SET_GLOBAL, OpCode::SET_GLOBAL_LONG)
        }
        else {
            (OpCode::GET_GLOBAL, OpCode::GET_GLOBAL_LONG)
        };

        if id > (u8::MAX as u16) {
            self.emit_byte(instruction_long);
            self.emit_bytes((id >> 8) as u8, (id & 0xFF) as u8);
        }
        else {
            self.emit_bytes(instruction, id as u8);
        }
    }

    fn emit_constant(&mut self, value: Value<'a>) {
        let prev_line = self.previous.line;
        if self.current_chunk().write_constant(value, prev_line).is_err() {
            self.error("Too many constants in one chunk.");
        }
    }
}


struct ParseRule {
    prefix: Option<fn(&mut Compiler, bool)>,
    infix: Option<fn(&mut Compiler)>,
    precedence: Precedence,
}

impl ParseRule {
    pub const fn new(prefix: Option<fn(&mut Compiler, bool)>, infix: Option<fn(&mut Compiler)>, precedence: Precedence) -> Self {
        Self {prefix, infix, precedence}
    } 
}

impl<'a> Compiler<'a> {
    fn get_rule(&self, kind: &TokenKind) -> &'static ParseRule {
        use TokenKind as TK;
        static LPAREN_RULE: ParseRule = ParseRule::new(Some(|s, _| s.grouping()), None, Precedence::None);

        static BITWISE_OR_RULE: ParseRule = ParseRule::new(None, Some(|s| s.binary()), Precedence::BitwiseOr);
        static BITWISE_XOR_RULE: ParseRule = ParseRule::new(None, Some(|s| s.binary()), Precedence::BitwiseXor);
        static BITWISE_AND_RULE: ParseRule = ParseRule::new(None, Some(|s| s.binary()), Precedence::BitwiseAnd);

        static PLUS_RULE: ParseRule = ParseRule::new(None, Some(|s| s.binary()), Precedence::Term);
        static MINUS_RULE: ParseRule = ParseRule::new(Some(|s, _| s.unary()), Some(|s| s.binary()), Precedence::Term);
        static UNARY_RULE: ParseRule = ParseRule::new(Some(|s, _| s.unary()), None, Precedence::None);
        static TERM_RULE: ParseRule = ParseRule::new(None, Some(|s| s.binary()), Precedence::Factor);

        static EQUALITY_RULE: ParseRule = ParseRule::new(None, Some(|s| s.binary()), Precedence::Equality);
        static COMPARISON_RULE: ParseRule = ParseRule::new(None, Some(|s| s.binary()), Precedence::Comparison);

        static NUMBER_RULE: ParseRule = ParseRule::new(Some(|s, _| s.emit_number()), None, Precedence::None);
        static LITERAL_RULE: ParseRule = ParseRule::new(Some(|s, _| s.emit_literal()), None, Precedence::None);
        static STRING_RULE: ParseRule = ParseRule::new(Some(|s, _| s.emit_string()), None, Precedence::None);
        static VARIABLE_RULE: ParseRule = ParseRule::new(Some(|s, b| s.emit_variable(b)), None, Precedence::None);
        static DEFAULT_RULE: ParseRule = ParseRule::new(None, None, Precedence::None);

        match kind {
            TK::LParen => &LPAREN_RULE,
            TK::Bar => &BITWISE_OR_RULE,
            TK::Carrot => &BITWISE_XOR_RULE,
            TK::And => &BITWISE_AND_RULE,
            TK::Plus => &PLUS_RULE,
            TK::Minus => &MINUS_RULE,
            TK::Exclamation => &UNARY_RULE,
            TK::Star | TK::Slash | TK::Rem => &TERM_RULE,
            TK::EqualEqual | TK::ExclamationEqual => &EQUALITY_RULE,
            TK::Greater | TK::GreaterEqual |TK::Less | TK::LessEqual => &COMPARISON_RULE,
            TK::Int | TK::Float => &NUMBER_RULE,
            TK::True | TK::False | TK::Nul => &LITERAL_RULE,
            TK::String => &STRING_RULE,
            TK::Identifier => &VARIABLE_RULE,
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

    fn syncronize(&mut self) {
        self.panic_mode = false;
        while self.current.kind != TokenKind::EOF {
            if self.previous.kind != TokenKind::Semicolon {return;}
            match self.current.kind {
                TokenKind::Print | TokenKind::Return | TokenKind::Let => return, // TODO: add more keywords maybe.
                _ => {}
            }
            self.advance();
        }
    }
}

