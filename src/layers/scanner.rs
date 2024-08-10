use std::collections::HashMap;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    Ident,

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
    BoolLiteral,

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
    keywords: HashMap<&'static str, TokenType>
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("and", TokenType::And);
        keywords.insert("or", TokenType::Or);
        keywords.insert("true", TokenType::BoolLiteral);
        keywords.insert("false", TokenType::BoolLiteral);
        Scanner {
            source,
            start: 0,
            current: 0,
            line: 1,
            keywords
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

    fn ident(&mut self) -> Token<'a> {
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let lexeme = self.lexeme();
        let token_type = self.keywords
            .get(lexeme)
            .copied()
            .unwrap_or(TokenType::Ident);
        self.make_token(token_type)
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
                    } else if c.is_alphabetic() {
                        self.ident()
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

