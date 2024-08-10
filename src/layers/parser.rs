use crate::layers::scanner::{Token, TokenType, Scanner};
use std::collections::HashMap;
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
    Not
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
            Operator::Not => "!"
        };
        write!(f, "{}", op_str)
    }
}

#[derive(Debug)]
pub enum Stmt {
    Print(Box<Expr>),
    ExprStmt(Box<Expr>),
    Block(Vec<Box<Stmt>>)
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_stmt(stmt: &Stmt, f: &mut fmt::Formatter<'_>, indent_level: usize) -> fmt::Result {
            let indent = "  ".repeat(indent_level);
            match stmt {
                Stmt::Print(expr) => {
                    writeln!(f, "{}Print(", indent)?;
                    writeln!(f, "{}  expr:", indent)?;
                    write!(f, "{}", expr)?;
                    writeln!(f, "\n{})", indent)
                }
                Stmt::ExprStmt(expr) => {
                    writeln!(f, "{}ExprStmt(", indent)?;
                    write!(f, "{}", expr)?;
                    writeln!(f, "\n{})", indent)
                }
                Stmt::Block(stmts) => {
                    writeln!(f, "{}Block {{", indent)?;
                    for stmt in stmts {
                        fmt_stmt(stmt, f, indent_level + 2)?;
                        writeln!(f)?;
                    }
                    write!(f, "{}}}", indent)
                }
            }
        }

        fmt_stmt(self, f, 0)
    }
}

#[derive(Debug)]
pub enum Expr {
    Number(f64),
    Bool(bool),
    Binary(Box<Expr>, Operator, Box<Expr>),
    Unary(Operator, Box<Expr>),
    Group(Box<Expr>)
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_expr(expr: &Expr, f: &mut fmt::Formatter<'_>, indent_level: usize) -> fmt::Result {
            let indent = "  ".repeat(indent_level);
            match expr {
                Expr::Number(value) => write!(f, "{}Number({})", indent, value),
                Expr::Bool(value) => write!(f, "{}Bool({})", indent, value),
                Expr::Binary(left, op, right) => {
                    writeln!(f, "{}Binary(", indent)?;
                    writeln!(f, "{}  left:", indent)?;
                    fmt_expr(left, f, indent_level + 2)?;
                    writeln!(f, ",")?;
                    writeln!(f, "{}  op: {:?}", indent, op)?;
                    writeln!(f, "{}  right:", indent)?;
                    fmt_expr(right, f, indent_level + 2)?;
                    write!(f, "\n{})", indent)
                }
                Expr::Unary(op, expr) => {
                    writeln!(f, "{}Unary(", indent)?;
                    writeln!(f, "{}  op: {:?}", indent, op)?;
                    writeln!(f, "{}  expr:", indent)?;
                    fmt_expr(expr, f, indent_level + 2)?;
                    write!(f, "\n{})", indent)
                }
                Expr::Group(expr) => {
                    writeln!(f, "{}Group(", indent)?;
                    writeln!(f, "{}  expr:", indent)?;
                    fmt_expr(expr, f, indent_level + 2)?;
                    write!(f, "\n{})", indent)
                }
            }
        }

        fmt_expr(self, f, 0)
    }
}

pub struct Program {
    pub stmts: Vec<Box<Stmt>>
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for expr in &self.stmts {
            writeln!(f, "{}", expr)?;
        }
        Ok(())
    }
}


pub struct Parser<'a> {
    pushed_token: Option<Token<'a>>,
    scanner: Scanner<'a>,
    errors: Vec<ParseError>,
    source: &'a str
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            pushed_token: None,
            scanner: Scanner::new(input),
            errors: vec![],
            source: input
        }
    }

    fn error(&mut self, token: Token, kind: ErrorType) -> ParseError {
        let message = match &kind {
            ErrorType::InvalidLiteral => format!("Failed to parse literal '{}'.", token.lexeme),
            ErrorType::ExpectedToken(expected) => format!("Expected {:?} but found '{}'", expected, token.lexeme),
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
                TokenType::Semicolon | TokenType::RBrace => {
                    self.push_back(token);
                    break;
                }
                _ => {
                    token = self.next();
                }
            }
        }
    }

    fn push_back(&mut self, token: Token<'a>) {
        self.pushed_token = Some(token);
    }

    fn next(&mut self) -> Token<'a> {
        if let Some(token) = self.pushed_token.take() {
            token
        } else {
            let token = self.scanner.next();
            match token.kind {
                _ => token
            }
        }
    }

    fn check(&mut self, expected: TokenType) -> bool {
        let t = self.next();
        let matches = t.kind == expected;
        self.push_back(t);
        matches
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.next();
        match token.kind {
            TokenType::NumericLiteral => {
                match token.lexeme.parse::<f64>() {
                    Ok(lit) => Ok(Expr::Number(lit)),
                    Err(_) => Err(self.error(token, ErrorType::InvalidLiteral))
                }
            },
            TokenType::BoolLiteral => {
                match token.lexeme.parse::<bool>() {
                    Ok(lit) => Ok(Expr::Bool(lit)),
                    Err(_) => Err(self.error(token, ErrorType::InvalidLiteral))
                }
            },
            TokenType::LParen => {
                let expression = self.expr(0)?;
                let token = self.next();
                if token.kind != TokenType::RParen {
                    let e = self.error(token, ErrorType::ExpectedToken(TokenType::RParen));
                    self.synchronize();
                    Err(e)
                } else {
                    Ok(Expr::Group(Box::new(expression)))
                }
            },
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
            },
            TokenType::Bang => {
                let operand = self.unary()?;
                Ok(Expr::Unary(Operator::Not, Box::new(operand)))
            },
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
        precedence_map.insert(TokenType::Minus, 4);
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
        let next_token = self.next();
        if next_token.kind != TokenType::Semicolon {
            let e = self.error(next_token, ErrorType::ExpectedToken(TokenType::Semicolon));
            self.synchronize();
            return Err(e);
        }
        Ok(Stmt::Print(Box::new(expr)))
    }

    fn stmt(&mut self) -> Result<Stmt, ParseError> {
        let token = self.next();
        match token.kind {
            TokenType::Print => self.print_stmt(),
            TokenType::LBrace => {
                let mut stmts = Vec::new();
                while !self.check(TokenType::RBrace) && !self.check(TokenType::EOF) {
                    stmts.push(Box::new(self.stmt()?));
                }
                let next_token = self.next();
                if next_token.kind != TokenType::RBrace {
                    let e = self.error(next_token, ErrorType::ExpectedToken(TokenType::RBrace));
                    self.synchronize();
                    Err(e)
                } else {
                    Ok(Stmt::Block(stmts))
                }
            }
            _ => {
                self.push_back(token);
                let expr = self.expr(0)?;
                let next_token = self.next();
                if next_token.kind != TokenType::Semicolon {
                    let e = self.error(next_token, ErrorType::ExpectedToken(TokenType::Semicolon));
                    self.synchronize();
                    Err(e)
                } else {
                    Ok(Stmt::ExprStmt(Box::new(expr)))
                }
            }
        }
    }

    fn program(&mut self) -> Result<Program, ParseError> {
        let mut stmts = Vec::new();
        while !self.check(TokenType::EOF) {
            stmts.push(Box::new(self.stmt()?));
        }
        Ok(Program{stmts})
    }

    pub fn parse(&mut self) -> Result<Program, Vec<ParseError>> {
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
