pub mod callable;
pub mod class;
pub use self::callable::{Fun, Call};
pub use self::class::LoxClass;

use std::fmt;
use crate::error::RuntimeError::*;
use crate::error::RuntimeError;
use std::rc::Rc;
use crate::value::class::LoxInstance;
use std::cell::RefCell;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    STRING(String),
    NUMBER(f64),
    BOOL(bool),
    FUN(Box<Fun>),
    CLASS(Rc<RefCell<LoxClass>>),
    INSTANCE(Rc<RefCell<LoxInstance>>),
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
            Value::CLASS(_) => { write!(f, "(class)") }
            Value::FUN(_) => { write!(f, "(fn)") }
            Value::NIL => { write!(f, "nil") }
            Value::INSTANCE(inst) => {write!(f, "{}", inst.borrow().to_string())}
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
            _ => Err(OperandMustNumber)
        }
    }
}

