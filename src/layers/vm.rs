pub enum OpCode {
    Add,
    Subtract,
    Multiply,
    Divide,
    Constant
}

pub struct VM {
    program: Vec<u8>,
    ip: usize,
    stack: [u8; 256],
    sp: usize,
}

impl VM {
    pub fn new() -> Self {
        VM {
            program: vec![],
            ip: 0,
            stack: [0; 256],
            sp: 0
        }
    }

    fn push(&mut self, byte: u8) {
        if self.sp == 255 {
            panic!("Stack Overflow!!!!");
        }
        self.stack[self.sp] = byte;
        self.sp += 1;
    }

    fn pop(&mut self) -> u8 {
        if self.sp == 0 {
            panic!("Stack Underflow!!!!!");
        }
        self.sp -= 1;
        self.stack[self.sp]
    }

    pub fn write_byte(&mut self, byte: u8) {
        self.program.push(byte);
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

    pub fn run(&mut self) {
        while let Some(byte) = self.read_byte() {
            println!("{:02X}", byte);
            match byte {
                x if x == OpCode::Add as u8 => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.wrapping_add(b));
                }
                x if x == OpCode::Subtract as u8 => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.wrapping_sub(b));
                }
                x if x == OpCode::Multiply as u8 => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.wrapping_mul(b));
                }
                x if x == OpCode::Divide as u8 => {
                    let b = self.pop();
                    let a = self.pop();
                    if b == 0 {
                        panic!("Division by 0!");
                    }
                    self.push(a / b);
                }
                _ => panic!("Unknown instruction: {:02X}", byte)
            }
        }
    }
}
