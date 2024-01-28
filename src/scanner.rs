#[derive(Debug)]
#[derive(PartialEq)]
pub enum TokenKind {
    // single character
    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,
    Plus, Minus, Slash,
    Dot, Comma, Colon, Semicolon,

    // one or two characters
    Exclamation, ExclamationEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    And, AndAnd,
    Bar, BarBar,
    Carrot, CarrotCarrot,
    Star, StarStar,

    // literals
    Identifier, String, Int,

    // special
    Error(String),
    EOF,
}

pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
    pub line: u32,
}

impl Token {
    pub fn new(start: usize, end: usize, line: u32, kind: TokenKind) -> Self {
        Self {
            kind,
            start,
            end,
            line,
        }
    }
    pub fn error(start: usize, end: usize, line: u32, msg: String) -> Self {
        Self {
            kind: TokenKind::Error(msg),
            start,
            end,
            line,
        }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }
    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return Token::new(self.start, self.current, self.line, TokenKind::EOF);
        }
        match self.advance() {
            '(' => Token::new(self.start, self.current, self.line, TokenKind::LParen),
            ')' => Token::new(self.start, self.current, self.line, TokenKind::RParen),
            '[' => Token::new(self.start, self.current, self.line, TokenKind::LBracket),
            ']' => Token::new(self.start, self.current, self.line, TokenKind::RBracket),
            '{' => Token::new(self.start, self.current, self.line, TokenKind::LBrace),
            '}' => Token::new(self.start, self.current, self.line, TokenKind::RBrace),
            '+' => Token::new(self.start, self.current, self.line, TokenKind::Plus),
            '-' => Token::new(self.start, self.current, self.line, TokenKind::Minus),
            '/' => Token::new(self.start, self.current, self.line, TokenKind::Slash),
            '.' => Token::new(self.start, self.current, self.line, TokenKind::Dot),
            ',' => Token::new(self.start, self.current, self.line, TokenKind::Comma),
            ':' => Token::new(self.start, self.current, self.line, TokenKind::Colon),
            ';' => Token::new(self.start, self.current, self.line, TokenKind::Semicolon),
            '!' => Token::new(self.start, self.current, self.line, if self.compare('=') {self.advance(); TokenKind::ExclamationEqual} else {TokenKind::Exclamation}), // ændre rækkefølge så først if, bagefter token::new(), ellers bliver den gamle current værdi brugt.
            '=' => Token::new(self.start, self.current, self.line, if self.compare('=') {self.advance(); TokenKind::EqualEqual} else {TokenKind::Equal}),
            '<' => Token::new(self.start, self.current, self.line, if self.compare('=') {self.advance(); TokenKind::LessEqual} else {TokenKind::Less}),
            '>' => Token::new(self.start, self.current, self.line, if self.compare('=') {self.advance(); TokenKind::GreaterEqual} else {TokenKind::Greater}),
            '&' => Token::new(self.start, self.current, self.line, if self.compare('&') {self.advance(); TokenKind::AndAnd} else {TokenKind::And}),
            '|' => Token::new(self.start, self.current, self.line, if self.compare('|') {self.advance(); TokenKind::BarBar} else {TokenKind::Bar}),
            '^' => Token::new(self.start, self.current, self.line, if self.compare('^') {self.advance(); TokenKind::CarrotCarrot} else {TokenKind::Carrot}),
            '*' => Token::new(self.start, self.current, self.line, if self.compare('*') {self.advance(); TokenKind::StarStar} else {TokenKind::Star}),
            _ => Token::error(self.start, self.current, self.line, String::from("bruh")),
        }
        
    }
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {self.advance();},
                '\n' => {self.line += 1; self.advance();}
                _ => break,
            }
        }
    }
    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }
    fn peek(&self) -> char {
        self.source.chars().nth(self.current).unwrap()
    }
    fn compare(&self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        expected == self.peek()
    }
    fn is_at_end(&self) -> bool {
        self.source.len() == self.current
    }
}