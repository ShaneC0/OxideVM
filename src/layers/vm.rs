use crate::layers::value::Value;

#[derive(Debug)]
pub enum OpCode {
    Constant = 0xF0,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    And,
    Not,
    Less,
    Equal,
    Swap,
    Print,
    Return
}

pub struct VM {
    program: Vec<u8>,
    constants: Vec<Value>,
    ip: usize,
    stack: [Value; 256],
    sp: usize,
}

impl VM {
    pub fn new(program: Vec<u8>, constants: Vec<Value>) -> Self {
        VM {
            program,
            constants,
            ip: 0,
            stack: [Value::Nil; 256],
            sp: 0
        }
    }

    fn push(&mut self,val: Value) {
        if self.sp == 255 {
            panic!("Stack Overflow!!!!");
        }
        self.stack[self.sp] = val;
        self.sp += 1;
    }

    fn pop(&mut self) -> Value {
        if self.sp == 0 {
            panic!("Stack Underflow!!!!!");
        }
        self.sp -= 1;
        self.stack[self.sp]
    }

    pub fn read_byte(&mut self) -> Option<u8> {
        if self.ip < self.program.len() {
            let b = self.program[self.ip];
            self.ip += 1;
            Some(b)
        } else {
            None
        }
    }

    fn print_stack(&self) {
        print!("Stack: [");
        for i in 0..self.sp {
            print!("{:?}", self.stack[i]);
            if i < self.sp - 1 {
                print!(", ");
            }
        }
        println!("]");
    }   

    fn binary_op(&mut self, op: fn(Value, Value) -> Value) {
        let b = self.pop();
        let a = self.pop();
        self.push(op(a, b));
    }

    fn unary_op(&mut self, op: fn(Value) -> Value) {
        let n = self.pop();
        self.push(op(n));
    }

    pub fn run(&mut self) {
        while let Some(byte) = self.read_byte() {
            self.print_stack();
            println!("{:02x}", byte);
            match byte {
                x if x == OpCode::Constant as u8 => {
                    if let Some(arg) = self.read_byte() {
                        self.push(self.constants[arg as usize]);
                    } 
                }
                x if x == OpCode::Add as u8 => self.binary_op(|a, b| a + b),
                x if x == OpCode::Subtract as u8 => self.binary_op(|a, b| a - b),
                x if x == OpCode::Multiply as u8 => self.binary_op(|a, b| a * b),
                x if x == OpCode::Divide as u8 => {
                    self.binary_op(|a, b| {
                        if b == Value::Float(0.0) {
                            panic!("Dividing by zero!");
                        }
                        a / b
                    });
                }
                x if x == OpCode::Negate as u8 => self.unary_op(|n| -n),
                x if x == OpCode::Not as u8 => self.unary_op(|n| !n),
                x if x == OpCode::And as u8 => self.binary_op(|a, b| a & b),
                x if x == OpCode::Less as u8 => self.binary_op(|a, b| Value::Boolean(a < b)),
                x if x == OpCode::Equal as u8 => self.binary_op(|a, b| Value::Boolean(a == b)),
                x if x == OpCode::Swap as u8 => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(b);
                    self.push(a);
                }
                x if x == OpCode::Print as u8 => println!("{}", self.pop()),
                x if x == OpCode::Return as u8 => {self.pop();},
                _ => panic!("Unknown instruction: {:02X}", byte)
            }
        }
    }
}
