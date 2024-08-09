use crate::layers::scanner::{Token, TokenType, Scanner};

pub enum ParseError {
    GenericError
}

pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate
}

pub enum Expr {
    Number(f64),
    Binary(Box<Expr>, Operator, Box<Expr>),
    Unary(Operator, Box<Expr>),
    Group(Box<Expr>)
}

pub struct Program {
    code: Vec<Expr>
}


pub struct Parser<'a> {
    pushed_token: Option<Token<'a>>,
    scanner: Scanner<'a>
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            pushed_token: None,
            scanner: Scanner::new(input)
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
                TokenType::Error => panic!("{} | Unexpected token {}.", token.line, token.lexeme.unwrap()),
                _ => token
            }
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.next();
        match token.kind {
            TokenType::NumericLiteral => {
                let literal: f64 = token.lexeme.expect("Expected literal!").parse().expect("Expected numeric literal!");
                Ok(Expr::Number(literal))
            },
            TokenType::LParen => {
                let expression = self.expr(0)?;
                let token = self.next();
                if token.kind != TokenType::RParen {
                    panic!("Expected closing bracket!");
                }
                Ok(Expr::Group(Box::new(expression)))
            },
            _ => panic!("Unexpected token in expression!!!!")
        }
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        let token = self.next();
        if token.kind == TokenType::Minus {
            let operand = self.unary()?;
            return Ok(Expr::Unary(Operator::Negate, Box::new(operand)));
        } else {
            self.push_back(token);
            self.primary()
        }
    }


    fn expr(&mut self, min_precedence: usize) -> Result<Expr, ParseError> {
        let mut lhs = self.unary()?;
        let precedences = vec![
            vec![TokenType::Star, TokenType::Slash],
            vec![TokenType::Plus, TokenType::Minus],
        ];
        loop {
            let op = self.next();
            let op_precedence = precedences.iter().position(|ops| ops.contains(&op.kind));
            if let Some(op_precedence) = op_precedence {
                if op_precedence < min_precedence {
                    self.push_back(op);
                    break;
                }
                let rhs = self.expr(op_precedence + 1)?; 
                lhs = Expr::Binary(
                    Box::new(lhs),
                    match op.kind {
                        TokenType::Star => Operator::Multiply,
                        TokenType::Plus => Operator::Add,
                        TokenType::Minus => Operator::Subtract,
                        TokenType::Slash => Operator::Divide,
                        _ => unreachable!()
                    },
                    Box::new(rhs)
                )
            } else {
                self.push_back(op);
                break;
            }
        }
        Ok(lhs)
    }

    fn program(&mut self) -> Result<Program, ParseError> {
        let mut exprs = Vec::new();
        loop {
            let token = self.next();
            if token.kind == TokenType::EOF {
                break;
            }
            self.push_back(token);
            let expr = self.expr(0)?;
            exprs.push(expr);
        }
        Ok(Program{code: exprs})
    }

    pub fn parse(&mut self) {
        let p = self.program();
        match p {
            Ok(p) => ,
            Err(e) => panic!("{}", e);
        }
    }
}
