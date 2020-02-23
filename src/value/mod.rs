pub mod callable;

pub use self::callable::{Fun, Call};

use std::fmt;
use crate::interpreter::RuntimeError::*;
use crate::interpreter::RuntimeError;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    STRING(String),
    NUMBER(f64),
    BOOL(bool),
    OBJECT,
    FUN(Fun),
    NIL,
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Value::NUMBER(v)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::STRING(s)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::STRING(String::from(s))
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::BOOL(b)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::STRING(s) => { write!(f, "{}", s.as_str()) }
            Value::NUMBER(v) => { write!(f, "{}", v) }
            Value::BOOL(b) => { write!(f, "{}", *b) }
            Value::OBJECT => { write!(f, "(object)") }
            Value::FUN(_) => { write!(f, "(fn)") }
            Value::NIL => { write!(f, "nil") }
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::BOOL(b) => *b,
            Value::STRING(s) => !s.is_empty(),
            Value::NUMBER(v) => *v != 0f64,
            Value::NIL => false,
            _ => true
        }
    }

    pub fn check_number(&self) -> Result<f64, RuntimeError> {
        match self {
            Value::NUMBER(v) => Ok(*v),
            _ => Err(OperandMustNumber {})
        }
    }
}

