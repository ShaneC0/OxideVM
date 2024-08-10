use std::ops::{Add, Sub, Mul, Div, Neg, Not, BitAnd};
use std::cmp::Ordering;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Float(f64),
    Boolean(bool),
    Nil,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(b),
            _ => None, // Returning None for non-comparable types
        }
    }

    fn lt(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => a < b,
            _ => panic!("Type error: Less than operation is only supported for Float values"),
        }
    }
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

impl Not for Value {
    type Output = Value;

    fn not(self) -> Value {
        match self {
            Value::Boolean(a) => Value::Boolean(!a),
            _ => panic!("Type error: Cannot apply NOT operator on non-boolean value"),
        }
    }
}

impl BitAnd for Value {
    type Output = Value;

    fn bitand(self, rhs: Value) -> Value {
        match (self, rhs) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a && b),
            _ => panic!("Cannot apply AND operator on non-boolean values!")
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
