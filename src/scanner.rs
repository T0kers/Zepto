#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // single character
    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,
    Plus, Minus, Slash, Rem, Tilde,
    Dot, Comma, Colon, Semicolon,

    // one or two characters
    Exclamation, ExclamationEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual, GreaterGreater,
    Less, LessEqual, LessLess,
    And, AndAnd,
    Bar, BarBar,
    Carrot, CarrotCarrot,
    Star, StarStar,

    // literals
    Identifier, String, Int, Float,

    // keywords
    If, Else, For, While,
	Let, Fn, Class, Selv,
	Print, Return,
	True, False,
	Nul,

    // special
    Error(String),
    EOF,
}

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
    pub line: u32,
}

impl Token {
    pub fn new(kind: TokenKind, start: usize, end: usize, line: u32) -> Self {
        Self {
            kind,
            start,
            end,
            line,
        }
    }
    pub fn new_identifier(source: &str, start: usize, end: usize, line: u32) -> Self {
        let mut chars = source[start..end].chars();
        let kind;
        if let Some(ch) = chars.nth(0) {
            kind = match ch {
                'c' => Token::check_keyword(chars.as_str(), "lass", TokenKind::Class),
                'i' => Token::check_keyword(chars.as_str(), "f", TokenKind::If),
                'e' => Token::check_keyword(chars.as_str(), "lse", TokenKind::Else),
                'f' => match chars.nth(0) {
                    Some('a') => Token::check_keyword(chars.as_str(), "lse", TokenKind::False),
                    Some('o') => Token::check_keyword(chars.as_str(), "r", TokenKind::For),
                    Some('n') => Token::check_keyword(chars.as_str(), "", TokenKind::Fn),
                    _ => TokenKind::Identifier,
                }
                'w' => Token::check_keyword(chars.as_str(), "hile", TokenKind::While),
                's' => Token::check_keyword(chars.as_str(), "elv", TokenKind::Selv),
                'l' => Token::check_keyword(chars.as_str(), "et", TokenKind::Let),
                'p' => Token::check_keyword(chars.as_str(), "rint", TokenKind::Print),
                'r' => Token::check_keyword(chars.as_str(), "eturn", TokenKind::Return),
                't' => Token::check_keyword(chars.as_str(), "rue", TokenKind::True),
                'n' => Token::check_keyword(chars.as_str(), "ul", TokenKind::Nul),
                _ => TokenKind::Identifier,
            };
        }
        else {
            kind = TokenKind::Identifier;
        }

        Self {
            kind, 
            start,
            end,
            line,
        }
    }
    fn check_keyword(rest: &str, check: &str, kind: TokenKind) -> TokenKind {
        if rest == check {kind}
        else {TokenKind::Identifier}
    }
    pub fn error(msg: String, start: usize, end: usize, line: u32) -> Self {
        Self {
            kind: TokenKind::Error(msg),
            start,
            end,
            line,
        }
    }
    pub fn lexeme<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start..self.end]
    }
}

pub struct Scanner<'a> {
    pub source: &'a str,
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
            return Token::new(TokenKind::EOF, self.start, self.current, self.line);
        }

        if let Some(ch) = self.advance() {
            match ch {
                '(' => Token::new(TokenKind::LParen, self.start, self.current, self.line),
                ')' => Token::new(TokenKind::RParen, self.start, self.current, self.line),
                '[' => Token::new(TokenKind::LBracket, self.start, self.current, self.line),
                ']' => Token::new(TokenKind::RBracket, self.start, self.current, self.line),
                '{' => Token::new(TokenKind::LBrace, self.start, self.current, self.line),
                '}' => Token::new(TokenKind::RBrace, self.start, self.current, self.line),
                '+' => Token::new(TokenKind::Plus, self.start, self.current, self.line),
                '-' => Token::new(TokenKind::Minus, self.start, self.current, self.line),
                '/' => Token::new(TokenKind::Slash, self.start, self.current, self.line),
                '%' => Token::new(TokenKind::Rem, self.start, self.current, self.line),
                '~' => Token::new(TokenKind::Tilde, self.start, self.current, self.line),
                '.' => Token::new(TokenKind::Dot, self.start, self.current, self.line),
                ',' => Token::new(TokenKind::Comma, self.start, self.current, self.line),
                ':' => Token::new(TokenKind::Colon, self.start, self.current, self.line),
                ';' => Token::new(TokenKind::Semicolon, self.start, self.current, self.line),
                '!' => Token::new( if self.compare('=') {
                    self.advance(); TokenKind::ExclamationEqual} else {TokenKind::Exclamation}, self.start, self.current, self.line),
                '=' => Token::new( if self.compare('=') {
                    self.advance(); TokenKind::EqualEqual} else {TokenKind::Equal}, self.start, self.current, self.line),
                '<' => Token::new( if self.compare('=') {
                    self.advance(); TokenKind::LessEqual
                } else if self.compare('<') {
                    self.advance(); TokenKind::LessLess
                } else {TokenKind::Less}, self.start, self.current, self.line),
                '>' => Token::new( if self.compare('=') {
                    self.advance(); TokenKind::GreaterEqual
                } else if self.compare('>') {
                    self.advance(); TokenKind::GreaterGreater
                } else {TokenKind::Greater}, self.start, self.current, self.line),
                '&' => Token::new( if self.compare('&') {
                    self.advance(); TokenKind::AndAnd} else {TokenKind::And}, self.start, self.current, self.line),
                '|' => Token::new( if self.compare('|') {
                    self.advance(); TokenKind::BarBar} else {TokenKind::Bar}, self.start, self.current, self.line),
                '^' => Token::new( if self.compare('^') {
                    self.advance(); TokenKind::CarrotCarrot} else {TokenKind::Carrot}, self.start, self.current, self.line),
                '*' => Token::new( if self.compare('*') {
                    self.advance(); TokenKind::StarStar} else {TokenKind::Star}, self.start, self.current, self.line),
                '"' => {
                    while self.peek() != Some('"') && !self.is_at_end() {
                        if let Some('\n') = self.peek() {self.line += 1}
                        self.advance();
                    }
                    if self.is_at_end() {return Token::error(String::from("Unterminated string."), self.start, self.current, self.line)};
                    self.advance();
                    Token::new(TokenKind::String, self.start, self.current, self.line)
                }
                ch if ch.is_ascii_digit() => {
                    while let Some(n) = self.peek() {
                        if n.is_ascii_digit() {self.advance();}
                        else {break;}
                    }
                    if let Some('.') = self.peek() {
                        if let Some(ch) = self.peek_next() {
                            if ch.is_ascii_digit() {
                                self.advance();
                                self.advance();
                                while let Some(n) = self.peek() {
                                    if n.is_ascii_digit() {self.advance();}
                                    else {break;}
                                }
                                return Token::new(TokenKind::Float, self.start, self.current, self.line);
                            }
                        }
                    }
                    Token::new(TokenKind::Int, self.start, self.current, self.line)
                }
                ch if ch.is_alphabetic() || ch == '_' => {
                    while let Some(next) = self.peek() {
                        if next.is_alphanumeric() || next == '|' {self.advance();}
                        else {break;}
                    }
                    Token::new_identifier(self.source, self.start, self.current, self.line)
                }
                _ => Token::error(String::from("bruh"), self.start, self.current, self.line),
            }
        }
        else {
            Token::error(String::from("Could not read token."), self.start, self.current, self.line)
        }
    }
    
    fn skip_whitespace(&mut self) {
        loop {
            if let Some(ch) = self.peek() {
                match ch {
                    ' ' | '\r' | '\t' => {self.advance();},
                    '\n' => {self.line += 1; self.advance();}
                    '/' => if self.peek_next() == Some('/') {
                        while !self.is_at_end() && self.peek() != Some('\n') {self.advance();}
                    }
                    else {
                        return;
                    }
                    _ => return,
                }
            }
            else {
                return;
            }
        }
    }
    
    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.source.chars().nth(self.current - 1)
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    fn peek_next(&self) -> Option<char> {
        if self.is_at_end() {None}
        else {self.source.chars().nth(self.current + 1)}
    }

    fn compare(&self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        Some(expected) == self.peek()
    }

    fn is_at_end(&self) -> bool {
        self.source.len() <= self.current
    }
}