use std::{collections::HashMap, io::{self, Write}};

use crate::{chunk::{Chunk, OpCode}, scanner::{Scanner, Token, TokenKind}, value::{Number, Value}, vm::VMError, function::Function};

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
    pub fn get_id(&self, name: &str) -> Option<u16> {
        self.global_ids.get(name).cloned()
    }
}

impl Default for Globals {
    fn default() -> Self {
        Self::new()
    }
}

struct ScopeInfo {
    count: u8,
    indexes: HashMap<String, u8>,
    not_initialized: String,
}

impl ScopeInfo {
    pub fn new() -> Self {
        Self {
            count: 0,
            indexes: HashMap::new(),
            not_initialized: String::new(),
        }
    }
}

pub enum CompileError {
    Overflow,
    DuplicateName,
}

pub enum FunctionKind {
    Function,
    Script,
}

pub struct Compiler {
    function: Function,
    kind: FunctionKind,

    next_index: u8,
    info: Vec<ScopeInfo>,
}

impl Compiler {
    pub fn new(kind: FunctionKind) -> Self {
        Self {
            function: Function::new(),
            kind,

            next_index: 1,
            info: vec![],
        }
    }
    pub fn begin_scope(&mut self) {
        self.info.push(ScopeInfo::new());
    }
    pub fn end_scope(&mut self) -> u8 {
        let count = self.info.pop().unwrap().count;
        self.next_index -= count;
        count
    }
    pub fn register_local(&mut self, name: &str) -> Result<(), CompileError> {
        let current_scope = self.info.last_mut().unwrap();
        if current_scope.indexes.get(name).is_some() {
            return Err(CompileError::DuplicateName);
        }
        current_scope.not_initialized = name.to_string();
        if self.next_index < u8::MAX { // IMPORTANT TODO: also check if there is space for tempoaries.
            current_scope.count += 1;
            self.next_index += 1;
        }
        else {
            return Err(CompileError::Overflow);
        }
        Ok(())
    }
    pub fn mark_as_initialized(&mut self) {
        let current_scope = self.info.last_mut().unwrap();
        let not_initialized = std::mem::take(&mut current_scope.not_initialized);
        current_scope.indexes.insert(not_initialized, self.next_index - 1);
    }
    pub fn get_index(&self, name: &str) -> Option<u8> {
        for i in 1..(self.depth() + 1) {
            let check_scope = &self.info[self.depth() - i];
            if let Some(index) = check_scope.indexes.get(name) {
                return Some(*index);
            }
        }
        None
    }

    pub fn depth(&self) -> usize {
        self.info.len()
    }
}

pub struct Parser<'a> {
    current: Token,
    previous: Token,
    scanner: Scanner<'a>,
    pub globals: Globals,
    compiler: Compiler,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, kind: FunctionKind) -> Self {
        Self {
            current: Token::new(TokenKind::Nul, 0, 0, 0),
            previous: Token::new(TokenKind::Nul, 0, 0, 0),
            scanner: Scanner::new(source),
            globals: Globals::new(),
            compiler: Compiler::new(kind),
            had_error: false,
            panic_mode: false,
        }
    }
    pub fn compile(&mut self) -> Result<&Function, VMError> {
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
            Ok(&self.compiler.function)
        }
    }
    pub fn current_chunk(&self) -> &Chunk {
        &self.compiler.function.chunk
    }
    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.compiler.function.chunk
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

impl<'a> Parser<'a> {
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
            TokenKind::Print => {self.advance(); self.print_statement()},
            TokenKind::Let => {self.advance(); self.let_statement()},
            TokenKind::LBrace => {self.advance(); self.block()},
            TokenKind::If => {self.advance(); self.if_statement()},
            TokenKind::While => {self.advance(); self.while_statement()},
            TokenKind::For => {self.advance(); self.for_statement()},
            // TokenKind::Fn => self.fn_statement(),
            // TokenKind::Return => self.return_statement(),
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
        self.expression();
        self.consume(TokenKind::Semicolon, "Expected ';' at end of print statement.");
        self.emit_byte(OpCode::PRINT);
    }
    fn let_statement(&mut self) {
        let id = self.parse_variable("Expected variable name.");
        if self.compare(TokenKind::Equal) {
            self.expression()
        }
        else {
            self.emit_byte(OpCode::NUL);
        }
        self.consume(TokenKind::Semicolon, "Expected ';' after variable declaration.");
        self.emit_define_variable(id);

    }
    fn begin_scope(&mut self) {
        self.compiler.begin_scope();
    }
    fn block(&mut self) {
        self.begin_scope();
        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::EOF) {
            self.statement();
        }
        self.consume(TokenKind::RBrace, "Expected '}' after block.");
        self.end_scope();
    }
    fn end_scope(&mut self) {
        let amount = self.compiler.end_scope();
        if amount > 0 {
            self.emit_bytes(OpCode::POP_SCOPE, amount);
        }
    }
    fn if_statement(&mut self) {
        self.expression();
        self.consume(TokenKind::LBrace, "Expected '{' after if statement.");
        let then_jump = self.emit_jump(OpCode::POP_JUMP_IF_FALSE);
        self.block();

        if self.compare(TokenKind::Else) {
            self.consume(TokenKind::LBrace, "Expected '{' after if statement.");
            let else_jump = self.emit_jump(OpCode::JUMP);

            self.patch_jump(then_jump);

            self.block();
            self.patch_jump(else_jump);
        }
        else {
            self.patch_jump(then_jump);
        }
    }
    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().code.len();
        self.expression();
        let exit_jump = self.emit_jump(OpCode::POP_JUMP_IF_FALSE);

        self.consume(TokenKind::LBrace, "Expected '{' after while statement.");
        self.block();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
    }
    fn for_statement(&mut self) {
        self.begin_scope();

        if self.compare(TokenKind::Semicolon) {
            // no initializer.
        }
        else if self.compare(TokenKind::Let) {
            self.let_statement();
        }
        else {
            self.expression_statement();
        }

        let mut loop_start = self.current_chunk().code.len();
        let mut exit_jump = None;
        if !self.compare(TokenKind::Semicolon) {
            self.expression();
            self.consume(TokenKind::Semicolon, "Expected ';' after for loop condition");
            exit_jump = Some(self.emit_jump(OpCode::POP_JUMP_IF_FALSE));
        }

        if !self.compare(TokenKind::LBrace) {
            let body_jump = self.emit_jump(OpCode::JUMP);
            let increment_start = self.current_chunk().code.len();
            self.expression();
            self.emit_byte(OpCode::POP);
            self.consume(TokenKind::LBrace, "Expected '{' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;

            self.patch_jump(body_jump);
        }
        
        self.block();

        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
        }

        self.end_scope();
    }
    pub fn parse_variable(&mut self, msg: &str) -> u16 {
        self.consume(TokenKind::Identifier, msg);

        self.declare_variable();
        if self.compiler.depth() > 0 {
            return 0;
        }
        self.globals.create_or_get_id(self.previous.lexeme(self.scanner.source))
    }
    fn declare_variable(&mut self) {
        if self.compiler.depth() == 0 {
            return;
        }
        if let Err(e) = self.compiler.register_local(self.previous.lexeme(self.scanner.source)) {
            match e {
                CompileError::Overflow => self.error("Too many local variables declared."),
                CompileError::DuplicateName => self.error("A variable with the same name already exists."),
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
    fn or(&mut self) {
        let end_jump = self.emit_jump(OpCode::JUMP_IF_TRUE);
        self.emit_byte(OpCode::POP);
        self.parse_precedence(Precedence::And);

        self.patch_jump(end_jump);
        if self.last_byte() != OpCode::BOOL {
            self.emit_byte(OpCode::BOOL);
        }
    }
    fn and(&mut self) {
        let end_jump = self.emit_jump(OpCode::JUMP_IF_FALSE);
        self.emit_byte(OpCode::POP);
        self.parse_precedence(Precedence::And);

        self.patch_jump(end_jump);
        if self.last_byte() != OpCode::BOOL {
            self.emit_byte(OpCode::BOOL);
        }
    }
}

// looking at bytecode
impl<'a> Parser<'a> {
    fn last_byte(&self) -> u8 {
        *self.current_chunk().code.last().unwrap()
    }
}

// emitting bytes
impl<'a> Parser<'a> {
    fn emit_byte(&mut self, byte: u8) {
        let prev_line = self.previous.line;
        self.current_chunk_mut().add_opcode(byte, prev_line);
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
        self.emit_constant(Value::Str(Box::new(
            self.previous.lexeme(self.scanner.source)[1..self.previous.lexeme(self.scanner.source).len() - 1].to_string() // TODO: make the amazing æ, ø and å work PLEASE!!!!!!!
        )));
    }
    fn emit_define_variable(&mut self, id: u16) {
        if self.compiler.depth() > 0 {
            self.compiler.mark_as_initialized();
            return;
        }
        if id > (u8::MAX as u16) {
            self.emit_byte(OpCode::DEFINE_GLOBAL_LONG);
            self.emit_bytes((id >> 8) as u8, (id & 0xFF) as u8);
        }
        else {
            self.emit_bytes(OpCode::DEFINE_GLOBAL, id as u8);
        }
    }
    fn emit_variable(&mut self, can_assign: bool) {
        let lexeme = self.previous.lexeme(self.scanner.source);
        if self.compiler.depth() > 0 {
            if let Some(index) = self.compiler.get_index(lexeme) {
                if can_assign && self.compare(TokenKind::Equal) {
                    self.expression();
                    self.emit_bytes(OpCode::SET_LOCAL, index);
                }
                else {
                    self.emit_bytes(OpCode::GET_LOCAL, index);
                }
                return;
            }
        }
        
        let id = self.globals.create_or_get_id(lexeme);

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

    fn emit_constant(&mut self, value: Value) {
        let prev_line = self.previous.line;
        if self.current_chunk_mut().write_constant(value, prev_line).is_err() {
            self.error("Too many constants in one chunk.");
        }
    }

    fn emit_jump(&mut self, instruction: u8) -> usize {
        self.emit_byte(instruction);
        self.emit_bytes(0xFF, 0xFF);
        self.current_chunk_mut().code.len() - 2
    }
    fn patch_jump(&mut self, offset: usize) {
        let jump = self.current_chunk_mut().code.len() - offset - 2;

        if jump > u16::MAX as usize {
            self.error("Then branch is too nig to jump over.");
            return;
        }
        self.current_chunk_mut().code[offset] = (jump >> 8) as u8;
        self.current_chunk_mut().code[offset + 1] = (jump & 0xFF) as u8;
    }
    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(OpCode::LOOP);
        let offset = self.current_chunk_mut().code.len() - loop_start + 2;
        if offset > u16::MAX as usize {
            self.error("Loop body is too large.");
        }
        self.emit_bytes((offset >> 8) as u8, (offset & 0xFF) as u8)
    }
}


struct ParseRule {
    prefix: Option<fn(&mut Parser, bool)>,
    infix: Option<fn(&mut Parser)>,
    precedence: Precedence,
}

impl ParseRule {
    pub const fn new(prefix: Option<fn(&mut Parser, bool)>, infix: Option<fn(&mut Parser)>, precedence: Precedence) -> Self {
        Self {prefix, infix, precedence}
    } 
}

impl<'a> Parser<'a> {
    fn get_rule(&self, kind: &TokenKind) -> &'static ParseRule {
        use TokenKind as TK;
        static LPAREN_RULE: ParseRule = ParseRule::new(Some(|s, _| s.grouping()), None, Precedence::None);

        static OR_RULE: ParseRule = ParseRule::new(None, Some(|s| s.or()), Precedence::Or);
        static AND_RULE: ParseRule = ParseRule::new(None, Some(|s| s.and()), Precedence::And);

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
            TK::BarBar => &OR_RULE,
            TK::AndAnd => &AND_RULE,
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
impl<'a> Parser<'a> {
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

