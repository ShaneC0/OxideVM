use crate::layers::scanner::{Scanner, Token, TokenType};
use std::collections::{HashMap, VecDeque};
use std::fmt;

#[derive(Debug, Clone)]
pub enum ErrorType {
    InvalidLiteral,
    ExpectedToken(TokenType),
    UnexpectedToken,
}

#[derive(Clone, Debug)]
pub struct ParseError {
    kind: ErrorType,
    message: String,
    line: usize,
    column: usize,
    lexeme: String,
}

impl ParseError {
    pub fn new(kind: ErrorType, token: &Token, message: String) -> Self {
        ParseError {
            kind,
            message,
            line: token.line,
            column: token.column,
            lexeme: token.lexeme.to_string(),
        }
    }

    pub fn display_with_context(&self, source: &str) -> String {
        let lines: Vec<&str> = source.lines().collect();
        let error_line = lines.get(self.line - 1).unwrap_or(&"");
        let pointer_line = format!("{:>width$}^", "", width = self.column);

        format!(
            "Error at line {}, column {}:\n{}\n{}\n{}",
            self.line, self.column, error_line, pointer_line, self.message
        )
    }
}

pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    And,
    Or,
    GThan,
    GEThan,
    LThan,
    LEThan,
    Equal,
    NotEqual,
    Not,
}

impl fmt::Debug for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op_str = match self {
            Operator::Add => "+",
            Operator::Subtract => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Negate => "-",
            Operator::And => "and",
            Operator::Or => "or",
            Operator::GThan => ">",
            Operator::GEThan => ">=",
            Operator::LThan => "<",
            Operator::LEThan => "<=",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
            Operator::Not => "!",
        };
        write!(f, "{}", op_str)
    }
}

#[derive(Debug)]
pub enum Decl {
    Var(String, Box<Option<Expr>>),
    Stmt(Box<Stmt>),
}

#[derive(Debug)]
pub enum Stmt {
    Print(Box<Expr>),
    Expr(Box<Expr>),
    Block(Vec<Box<Decl>>),
    Assign(String, Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Expr>, Box<Stmt>),
}

#[derive(Debug)]
pub enum Expr {
    Number(f64),
    Bool(bool),
    String(String),
    Ident(String),
    Binary(Box<Expr>, Operator, Box<Expr>),
    Unary(Operator, Box<Expr>),
    Group(Box<Expr>),
}

pub struct AST {
    pub decls: Vec<Box<Decl>>,
}

pub struct Parser<'a> {
    pushed_tokens: VecDeque<Token<'a>>,
    scanner: Scanner<'a>,
    errors: Vec<ParseError>,
    source: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            pushed_tokens: VecDeque::new(),
            scanner: Scanner::new(input),
            errors: vec![],
            source: input,
        }
    }

    fn error(&mut self, token: Token, kind: ErrorType) -> ParseError {
        let message = match &kind {
            ErrorType::InvalidLiteral => format!("Failed to parse literal '{}'.", token.lexeme),
            ErrorType::ExpectedToken(expected) => {
                format!("Expected {:?} but found '{}'", expected, token.lexeme)
            }
            ErrorType::UnexpectedToken => format!("Unexpected token '{}'", token.lexeme),
        };
        let error = ParseError::new(kind, &token, message);
        eprintln!("{}", error.display_with_context(self.source));
        self.errors.push(error.clone());
        error
    }

    fn synchronize(&mut self) {
        let mut token = self.next();
        loop {
            match token.kind {
                TokenType::Semicolon | TokenType::RBrace | TokenType::EOF => {
                    break;
                }
                _ => {
                    token = self.next();
                }
            }
        }
    }

    fn push_back(&mut self, token: Token<'a>) {
        self.pushed_tokens.push_back(token);
    }

    fn next(&mut self) -> Token<'a> {
        if let Some(token) = self.pushed_tokens.pop_front() {
            token
        } else {
            self.scanner.next()
        }
    }

    fn check(&mut self, expected: TokenType) -> bool {
        let t = self.next();
        let matches = t.kind == expected;
        self.push_back(t);
        matches
    }

    fn consume(&mut self, kind: TokenType) -> Result<(), ParseError> {
        let t = self.next();
        if t.kind == kind {
            Ok(())
        } else {
            let e = self.error(t, ErrorType::ExpectedToken(kind));
            self.synchronize();
            Err(e)
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.next();
        match token.kind {
            TokenType::NumericLiteral => match token.lexeme.parse::<f64>() {
                Ok(lit) => Ok(Expr::Number(lit)),
                Err(_) => Err(self.error(token, ErrorType::InvalidLiteral)),
            },
            TokenType::BoolLiteral => match token.lexeme.parse::<bool>() {
                Ok(lit) => Ok(Expr::Bool(lit)),
                Err(_) => Err(self.error(token, ErrorType::InvalidLiteral)),
            },
            TokenType::StringLiteral => Ok(Expr::String(token.lexeme.to_string())),
            TokenType::LParen => {
                let expression = self.expr(0)?;
                self.consume(TokenType::RParen)?;
                Ok(Expr::Group(Box::new(expression)))
            }
            TokenType::Identifier => Ok(Expr::Ident(token.lexeme.to_string())),
            _ => {
                let e = self.error(token, ErrorType::UnexpectedToken);
                self.synchronize();
                Err(e)
            }
        }
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        let token = self.next();
        match token.kind {
            TokenType::Minus => {
                let operand = self.unary()?;
                Ok(Expr::Unary(Operator::Negate, Box::new(operand)))
            }
            TokenType::Bang => {
                let operand = self.unary()?;
                Ok(Expr::Unary(Operator::Not, Box::new(operand)))
            }
            _ => {
                self.push_back(token);
                self.primary()
            }
        }
    }

    fn expr(&mut self, min_precedence: usize) -> Result<Expr, ParseError> {
        let mut lhs = self.unary()?;
        let mut precedence_map: HashMap<TokenType, usize> = std::collections::HashMap::new();
        precedence_map.insert(TokenType::And, 1);
        precedence_map.insert(TokenType::Or, 1);
        precedence_map.insert(TokenType::EqualEqual, 2);
        precedence_map.insert(TokenType::BangEqual, 2);
        precedence_map.insert(TokenType::Greater, 3);
        precedence_map.insert(TokenType::GreaterEqual, 3);
        precedence_map.insert(TokenType::Less, 3);
        precedence_map.insert(TokenType::LessEqual, 3);
        precedence_map.insert(TokenType::Plus, 4);
        precedence_map.insert(TokenType::Minus, 5);
        precedence_map.insert(TokenType::Star, 5);
        precedence_map.insert(TokenType::Slash, 5);
        loop {
            let next_token = self.next();
            if let Some(&op_precedence) = precedence_map.get(&next_token.kind) {
                if op_precedence < min_precedence {
                    self.push_back(next_token);
                    break;
                }
                let rhs = self.expr(op_precedence + 1)?;
                lhs = Expr::Binary(
                    Box::new(lhs),
                    match next_token.kind {
                        TokenType::Star => Operator::Multiply,
                        TokenType::Plus => Operator::Add,
                        TokenType::Minus => Operator::Subtract,
                        TokenType::Slash => Operator::Divide,
                        TokenType::And => Operator::And,
                        TokenType::Or => Operator::Or,
                        TokenType::EqualEqual => Operator::Equal,
                        TokenType::BangEqual => Operator::NotEqual,
                        TokenType::Greater => Operator::GThan,
                        TokenType::GreaterEqual => Operator::GEThan,
                        TokenType::Less => Operator::LThan,
                        TokenType::LessEqual => Operator::LEThan,
                        _ => {
                            let e = self.error(next_token, ErrorType::UnexpectedToken);
                            self.synchronize();
                            return Err(e);
                        }
                    },
                    Box::new(rhs),
                );
            } else {
                self.push_back(next_token);
                break;
            }
        }
        Ok(lhs)
    }

    fn print_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expr(0)?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Print(Box::new(expr)))
    }

    fn expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expr(0)?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Expr(Box::new(expr)))
    }

    fn block_stmt(&mut self) -> Result<Stmt, ParseError> {
        let mut decls = Vec::new();
        while !self.check(TokenType::RBrace) && !self.check(TokenType::EOF) {
            decls.push(Box::new(self.decl()?));
        }
        let next_token = self.next();
        if next_token.kind != TokenType::RBrace {
            let e = self.error(next_token, ErrorType::ExpectedToken(TokenType::RBrace));
            self.synchronize();
            Err(e)
        } else {
            Ok(Stmt::Block(decls))
        }
    }

    fn assign_stmt(&mut self) -> Result<Stmt, ParseError> {
        let ident = self.next();
        self.next();
        let expr = self.expr(0)?;
        let semi = self.next();
        if semi.kind != TokenType::Semicolon {
            let e = self.error(semi, ErrorType::ExpectedToken(TokenType::Semicolon));
            self.synchronize();
            Err(e)
        } else {
            Ok(Stmt::Assign(ident.lexeme.to_string(), Box::new(expr)))
        }
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenType::LParen)?;
        let expr = self.expr(0)?;
        self.consume(TokenType::RParen)?;
        let then = self.stmt()?;
        let next = self.next();
        if next.kind != TokenType::Else {
            self.push_back(next);
            return Ok(Stmt::If(Box::new(expr), Box::new(then), None));
        }
        let otherwise = self.stmt()?;
        Ok(Stmt::If(Box::new(expr), Box::new(then), Some(Box::new(otherwise))))
    }

    fn while_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenType::LParen)?;
        let expr = self.expr(0)?;
        self.consume(TokenType::RParen)?;
        let body = self.stmt()?;
        Ok(Stmt::While(Box::new(expr), Box::new(body)))
    }

    fn stmt(&mut self) -> Result<Stmt, ParseError> {
        let token = self.next();
        match token.kind {
            TokenType::Print => self.print_stmt(),
            TokenType::LBrace => self.block_stmt(),
            TokenType::If => self.if_stmt(),
            TokenType::While => self.while_stmt(),
            TokenType::Identifier => {
                let next_token = self.next();
                if next_token.kind == TokenType::Equal {
                    self.push_back(token);
                    self.push_back(next_token);
                    self.assign_stmt()
                } else {
                    self.push_back(token);
                    self.push_back(next_token);
                    self.expr_stmt()
                }
            }
            _ => {
                self.push_back(token);
                self.expr_stmt()
            }
        }
    }

    fn let_decl(&mut self) -> Result<Decl, ParseError> {
        let ident = self.next();
        if ident.kind != TokenType::Identifier {
            let e = self.error(ident, ErrorType::ExpectedToken(TokenType::Identifier));
            self.synchronize();
            return Err(e);
        }
        let name = ident.lexeme.to_string();
        let token = self.next();
        let initializer: Option<Expr>;
        if token.kind == TokenType::Equal {
            initializer = Some(self.expr(0)?);
        } else {
            self.push_back(token);
            initializer = None;
        }
        self.consume(TokenType::Semicolon)?;
        Ok(Decl::Var(name, Box::new(initializer)))
    }

    fn decl(&mut self) -> Result<Decl, ParseError> {
        let next_token = self.next();
        if next_token.kind == TokenType::Let {
            self.let_decl()
        } else {
            self.push_back(next_token);
            let stmt = self.stmt()?;
            Ok(Decl::Stmt(Box::new(stmt)))
        }
    }

    fn program(&mut self) -> Result<AST, ParseError> {
        let mut decls = Vec::new();
        while !self.check(TokenType::EOF) {
            decls.push(Box::new(self.decl()?));
        }
        Ok(AST { decls })
    }

    pub fn parse(&mut self) -> Result<AST, Vec<ParseError>> {
        match self.program() {
            Ok(p) => {
                if self.errors.is_empty() {
                    Ok(p)
                } else {
                    Err(self.errors.clone())
                }
            }
            Err(_) => {
                self.synchronize();
                Err(self.errors.clone())
            }
        }
    }
}
