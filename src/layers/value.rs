use std::ops::{Add, Sub, Mul, Div, Neg};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Value {
    Float(f64),
    Boolean(bool),
    Nil,
}

impl Add for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            _ => panic!("Type error: Addition is only supported for Float values"),
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, other: Value) -> Value {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            _ => panic!("Type error: Subtraction is only supported for Float values"),
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            _ => panic!("Type error: Multiplication is only supported for Float values"),
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => {
                if b == 0.0 {
                    panic!("Division by zero error");
                }
                Value::Float(a / b)
            }
            _ => panic!("Type error: Division is only supported for Float values"),
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self {
            Value::Float(a) => Value::Float(-a),
            _ => panic!("Type error: Negation is only supported for Float values"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Float(v) => write!(f, "{}", v),
            Value::Boolean(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "Nil"),
        }
    }
}
