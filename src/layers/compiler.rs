use crate::layers::parser::{Expr, Program, Operator};
use crate::layers::vm::{OpCode};
use crate::layers::value::{Value};


pub struct Compiler {
    pub chunk: Vec<u8>,
    pub constants: Vec<Value>
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            chunk: vec![],
            constants: vec![]
        }
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.push(byte);
    }

    fn store_constant(&mut self, val: Value) -> u8 {
        if self.constants.len() == 255 {
            panic!("Maximum constant limit reached (255).");
        }
        self.constants.push(val);
        (self.constants.len() - 1).try_into().unwrap()
    }

    pub fn compile_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Number(literal) => {
                self.emit_byte(OpCode::Constant as u8);
                let const_idx = self.store_constant(Value::Float(literal));
                self.emit_byte(const_idx);
            },
            Expr::Bool(literal) => {
                self.emit_byte(OpCode::Constant as u8);
                let const_idx = self.store_constant(Value::Boolean(literal));
                self.emit_byte(const_idx);
            }
            Expr::Binary(left, op, right) => {
                self.compile_expr(*left);
                self.compile_expr(*right);
                match op {
                    Operator::Add => self.emit_byte(OpCode::Add as u8),
                    Operator::Subtract => self.emit_byte(OpCode::Subtract as u8),
                    Operator::Multiply => self.emit_byte(OpCode::Multiply as u8),
                    Operator::Divide => self.emit_byte(OpCode::Divide as u8),
                    Operator::And => self.emit_byte(OpCode::And as u8),
                    Operator::Or => self.emit_byte(OpCode::Or as u8),
                    Operator::GThan => self.emit_byte(OpCode::Greater as u8),
                    Operator::GEThan => self.emit_byte(OpCode::GreaterEqual as u8),
                    Operator::LThan => self.emit_byte(OpCode::Less as u8),
                    Operator::LEThan => self.emit_byte(OpCode::LessEqual as u8),
                    Operator::Equal => self.emit_byte(OpCode::Equal as u8),
                    Operator::NotEqual => self.emit_byte(OpCode::NotEqual as u8),
                    _ => unreachable!()
                }
            },
            Expr::Unary(op, operand) => {
                self.compile_expr(*operand);
                match op {
                    Operator::Negate => self.emit_byte(OpCode::Negate as u8),
                    Operator::Not => self.emit_byte(OpCode::Not as u8),
                    _ => unreachable!()
                }
            },
            Expr::Group(expression) => self.compile_expr(*expression),
        } 
    }

    pub fn compile(&mut self, ast: Expr) {
        self.compile_expr(ast);
        self.emit_byte(OpCode::Return as u8);
    }
}
