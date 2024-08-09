#[derive(Debug, PartialEq)]
pub enum TokenType {
    Equal,

    Or,
    And,

    EqualEqual,
    BangEqual,

    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Plus,
    Minus,

    Star,
    Slash,

    Bang,

    LParen,
    RParen,

    NumericLiteral,

    Error,
    EOF,
}

pub struct Token<'a> {
    pub kind: TokenType,
    pub lexeme: &'a str,
    pub line: u32,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenType, lexeme: &'a str, line: u32) -> Self {
        Token { kind, lexeme, line }
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
        Scanner {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    fn advance(&mut self) {
        if self.current < self.source.len() {
            self.current += 1;
        }
    }

    fn lexeme(&self) -> &'a str {
        &self.source[self.start..self.current]
    }

    fn make_token(&mut self, kind: TokenType) -> Token<'a> {
        let t = Token::new(
            kind,
            self.lexeme(),
            self.line,
        );
        self.start = self.current;
        t
    }

    fn check(&mut self, target: char) -> bool {
        if self.source.chars().nth(self.current).unwrap() == target {
            self.advance();
            return true;
        }
        false
    }

    fn error_token(&mut self) -> Token<'a> {
        Token::new(TokenType::Error, self.lexeme(), self.line)
    }

    fn number(&mut self) -> Token<'a> {
        let mut dot_seen = false;
        while let Some(c) = self.peek() {
            if c == '.' {
                if dot_seen {
                    break; 
                }
                dot_seen = true;
                self.advance();
            } else if c.is_numeric() {
                self.advance();
            } else {
                break;
            }
        }
        self.make_token(TokenType::NumericLiteral)
    }
    
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                ' ' | '\r' | '\t' => self.advance(), 
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                _ => break,
            }
        }
        self.start = self.current;
    }

    pub fn next(&mut self) -> Token<'a> {
        self.skip_whitespace();
        if let Some(c) = self.peek() {
            self.advance();
            match c {
                '(' => self.make_token(TokenType::LParen),
                ')' => self.make_token(TokenType::RParen),
                '+' => self.make_token(TokenType::Plus),
                '-' => self.make_token(TokenType::Minus),
                '*' => self.make_token(TokenType::Star),
                '/' => self.make_token(TokenType::Slash),
                '!' => {
                    if self.check('=') {
                        self.make_token(TokenType::BangEqual)
                    } else {
                        self.make_token(TokenType::Bang)
                    }
                }
                '=' => {
                    if self.check('=') {
                        self.make_token(TokenType::EqualEqual)
                    } else {
                        self.make_token(TokenType::Equal)
                    }
                }
                '>' => {
                    if self.check('=') {
                        self.make_token(TokenType::GreaterEqual)
                    } else {
                        self.make_token(TokenType::Greater)
                    }
                }
                '<' => {
                    if self.check('=') {
                        self.make_token(TokenType::LessEqual)
                    } else {
                        self.make_token(TokenType::Less)
                    }
                }
                _ => {
                    if c.is_numeric() {
                        self.number()
                    } else {
                        self.error_token()
                    }
                }
            }
        } else {
            Token::new(TokenType::EOF, "", self.line)
        }
    }
}

