use super::*;
use std::fmt::{Debug, Formatter};
use crate::parser::Stmt;

type NativeFn = fn(args: Vec<Value>) -> Value;

pub trait Call {
    fn call(&mut self, args: Vec<Value>) -> Value;
}


type Func = Rc<dyn FnMut(Vec<Value>) -> Value>;
#[derive(Clone, PartialEq)]
pub enum Fun {
    Native(String, i8, NativeFn),
    UserFunc(String, i8, Stmt)
}



impl Debug for Fun {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name())
    }
}


impl Fun {

    pub fn arity(&self) -> i8 {
        match self {
            Fun::Native(_, arity, _) => *arity,
            Fun::UserFunc(_, arity, _) => *arity,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Fun::Native(name, _, _) => name.as_str(),
            Fun::UserFunc(name, _, _) => name.as_str(),
        }
    }
}


impl From<Fun> for Value {
    fn from(f: Fun) -> Self {
        Value::FUN(f)
    }
}

