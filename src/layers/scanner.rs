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
    pub lexeme: Option<&'a str>,
    pub line: u32,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenType, lexeme: Option<&'a str>, line: u32) -> Self {
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

    fn make_token(&mut self, kind: TokenType, has_lexeme: bool) -> Token<'a> {
        let t = Token::new(
            kind,
            if has_lexeme {
                Some(self.lexeme())
            } else {
                None
            },
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
        Token::new(TokenType::Error, Some(self.lexeme()), self.line)
    }

    fn number(&mut self) -> Token<'a> {
        let mut dot_seen = false;
        while let Some(c) = self.peek() {
            if c == '.' {
                if dot_seen {
                    return self.error_token();
                }
                dot_seen = true;
            } else if c.is_numeric() {
                self.advance();
            } else {
                break;
            }
        }
        self.make_token(TokenType::NumericLiteral, true)
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
                '(' => self.make_token(TokenType::LParen, false),
                ')' => self.make_token(TokenType::RParen, false),
                '+' => self.make_token(TokenType::Plus, false),
                '-' => self.make_token(TokenType::Minus, false),
                '*' => self.make_token(TokenType::Star, false),
                '/' => self.make_token(TokenType::Slash, false),
                '!' => {
                    if self.check('=') {
                        self.make_token(TokenType::BangEqual, false)
                    } else {
                        self.make_token(TokenType::Bang, false)
                    }
                }
                '=' => {
                    if self.check('=') {
                        self.make_token(TokenType::EqualEqual, false)
                    } else {
                        self.make_token(TokenType::Equal, false)
                    }
                }
                '>' => {
                    if self.check('=') {
                        self.make_token(TokenType::GreaterEqual, false)
                    } else {
                        self.make_token(TokenType::Greater, false)
                    }
                }
                '<' => {
                    if self.check('=') {
                        self.make_token(TokenType::LessEqual, false)
                    } else {
                        self.make_token(TokenType::Less, false)
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
            Token::new(TokenType::EOF, None, self.line)
        }
    }
}

