use crate::layers::interner::HeapInterner;
use crate::layers::parser::{Decl, Expr, Operator, Stmt, AST};
use crate::layers::value::Value;
use crate::layers::vm::OpCode;

pub struct Compiler {
    pub chunk: Vec<u8>,
    pub constants: Vec<Value>,
    pub interner: HeapInterner,
    pub locals: Vec<(String, i32)>,
    pub scope_depth: i32,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            chunk: vec![],
            constants: vec![],
            interner: HeapInterner::new(),
            locals: vec![],
            scope_depth: 0,
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

    fn resolve_local(&self, name: &str) -> i32 {
        for (idx, local) in self.locals.iter().rev().enumerate() {
            if local.0 == name {
                return idx.try_into().unwrap();
            }
        }
        return -1;
    }

    pub fn compile_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Number(literal) => {
                self.emit_byte(OpCode::Constant as u8);
                let const_idx = self.store_constant(Value::Float(literal));
                self.emit_byte(const_idx);
            }
            Expr::Bool(literal) => {
                self.emit_byte(OpCode::Constant as u8);
                let const_idx = self.store_constant(Value::Boolean(literal));
                self.emit_byte(const_idx);
            }
            Expr::String(literal) => {
                self.emit_byte(OpCode::Constant as u8);
                let interned_literal = self.interner.intern(literal);
                let const_idx = self.store_constant(Value::String(interned_literal));
                self.emit_byte(const_idx);
            }
            Expr::Ident(name) => {
                let arg = self.resolve_local(&name);
                if arg != -1 {
                    self.emit_byte(OpCode::GetLocal as u8);
                    self.emit_byte(arg.try_into().unwrap());
                } else {
                    self.emit_byte(OpCode::GetGlobal as u8);
                    let interned_name = self.interner.intern(name);
                    let const_idx = self.store_constant(Value::String(interned_name));
                    self.emit_byte(const_idx);
                }
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
                    Operator::Or => {
                        self.emit_byte(OpCode::Not as u8);
                        self.emit_byte(OpCode::Swap as u8);
                        self.emit_byte(OpCode::Not as u8);
                        self.emit_byte(OpCode::And as u8);
                        self.emit_byte(OpCode::Not as u8);
                    }
                    Operator::GThan => {
                        self.emit_byte(OpCode::Swap as u8);
                        self.emit_byte(OpCode::Less as u8);
                    }
                    Operator::GEThan => {
                        self.emit_byte(OpCode::Less as u8);
                        self.emit_byte(OpCode::Not as u8);
                    }
                    Operator::LThan => self.emit_byte(OpCode::Less as u8),
                    Operator::LEThan => {
                        self.emit_byte(OpCode::Swap as u8);
                        self.emit_byte(OpCode::Less as u8);
                        self.emit_byte(OpCode::Not as u8);
                    }
                    Operator::Equal => self.emit_byte(OpCode::Equal as u8),
                    Operator::NotEqual => {
                        self.emit_byte(OpCode::Equal as u8);
                        self.emit_byte(OpCode::Not as u8);
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Unary(op, operand) => {
                self.compile_expr(*operand);
                match op {
                    Operator::Negate => self.emit_byte(OpCode::Negate as u8),
                    Operator::Not => self.emit_byte(OpCode::Not as u8),
                    _ => unreachable!(),
                }
            }
            Expr::Group(expression) => self.compile_expr(*expression),
        }
    }

    fn emit_jump(&mut self, kind: OpCode) -> usize {
        self.emit_byte(kind as u8);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        return self.chunk.len() - 2;
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump: u16 = (self.chunk.len() - offset - 2).try_into().unwrap();
        self.chunk[offset] = (jump >> 8) as u8;
        self.chunk[offset + 1] = (jump & 0xff) as u8;
    }

    fn emit_loop(&mut self, start: usize) {
        self.emit_byte(OpCode::Loop as u8);
        let jump: u16 = (self.chunk.len() - start + 2).try_into().unwrap();
        if jump > 65534 {
            panic!("Loop body too large!");
        }
        self.emit_byte((jump >> 8) as u8);
        self.emit_byte((jump & 0xff) as u8)
    }

    pub fn compile_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Print(expr) => {
                self.compile_expr(*expr);
                self.emit_byte(OpCode::Print as u8);
            }
            Stmt::If(condition, then, otherwise) => {
                self.compile_expr(*condition);
                let then_jump = self.emit_jump(OpCode::JumpIfFalse);
                self.emit_byte(OpCode::Pop as u8);
                self.compile_stmt(*then);
                let else_jump = self.emit_jump(OpCode::Jump);
                self.patch_jump(then_jump);
                self.emit_byte(OpCode::Pop as u8);
                if let Some(body) = otherwise {
                    self.compile_stmt(*body);
                }
                self.patch_jump(else_jump);
            }
            Stmt::While(condition, body) => {
                let loop_start = self.chunk.len();
                self.compile_expr(*condition);
                let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
                self.emit_byte(OpCode::Pop as u8);
                self.compile_stmt(*body);
                self.emit_loop(loop_start);
                self.patch_jump(exit_jump);
                self.emit_byte(OpCode::Pop as u8);
            }

            Stmt::Expr(expr) => {
                self.compile_expr(*expr);
                self.emit_byte(OpCode::Pop as u8);
            }
            Stmt::Assign(ident, expr) => {
                self.compile_expr(*expr);
                let arg = self.resolve_local(&ident);
                if arg != -1 {
                    self.emit_byte(OpCode::SetLocal as u8);
                    self.emit_byte(arg.try_into().unwrap())
                } else {
                    self.emit_byte(OpCode::SetGlobal as u8);
                    let interned_name = self.interner.intern(ident);
                    let name_idx = self.store_constant(Value::String(interned_name));
                    self.emit_byte(name_idx);
                }
            }
            Stmt::Block(decls) => {
                self.scope_depth += 1;
                self.compile_prog(decls);
                self.scope_depth -= 1;
                while self.locals.len() > 0
                    && self.locals[self.locals.len() - 1].1 > self.scope_depth
                {
                    self.emit_byte(OpCode::Pop as u8);
                    self.locals.pop();
                }
            }
        }
    }

    pub fn compile_decl(&mut self, decl: Decl) {
        match decl {
            Decl::Var(name, initializer) => {
                if let Some(expr) = *initializer {
                    self.compile_expr(expr);
                } else {
                    self.emit_byte(OpCode::Nil as u8);
                }
                if self.scope_depth > 0 {
                    if self.locals.len() == 255 {
                        panic!("Compiler error: Too many locals!");
                    }
                    for local in self.locals.iter().rev() {
                        if local.1 != -1 && local.1 < self.scope_depth {
                            break;
                        }
                        if name == local.0 {
                            panic!("Attempt to redefine local variable {}", local.0);
                        }
                    }
                    self.locals.push((name, self.scope_depth));
                } else {
                    let interned_name = self.interner.intern(name);
                    let name_idx = self.store_constant(Value::String(interned_name));
                    self.emit_byte(OpCode::DefineGlobal as u8);
                    self.emit_byte(name_idx);
                }
            }
            Decl::Stmt(stmt) => self.compile_stmt(*stmt),
        }
    }

    pub fn compile_prog(&mut self, decls: Vec<Box<Decl>>) {
        for decl in decls {
            self.compile_decl(*decl);
        }
    }

    pub fn compile(&mut self, ast: AST) {
        self.compile_prog(ast.decls);
    }
}
