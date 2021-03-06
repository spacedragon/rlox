use super::*;
use std::fmt::{Debug, Formatter};
use crate::parser::Stmt;
use crate::environment::Environment;
use std::cell::RefCell;

type NativeFn = fn(args: Vec<Value>) -> Value;

pub trait Call {
    fn call(&mut self, args: Vec<Value>) -> Value;
}


#[derive(Clone, PartialEq)]
pub enum Fun {
    Native(String, u8, NativeFn),
    UserFunc(String, u8, Stmt, Rc<RefCell<Environment>>)
}



impl Debug for Fun {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name())
    }
}


impl Fun {

    pub fn arity(&self) -> u8 {
        match self {
            Fun::Native(_, arity, _) => *arity,
            Fun::UserFunc(_, arity, _, _) => *arity,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Fun::Native(name, _, _) => name.as_str(),
            Fun::UserFunc(name, _, _,_) => name.as_str(),
        }
    }

    pub fn bind(&mut self, inst: Rc<RefCell<LoxInstance>>) {
        if let Fun::UserFunc(_, _, _, env) = self {
            let mut new_env = Environment::new(env.clone());
            new_env.define("this".to_string(), Value::INSTANCE(inst));
            *env = Rc::new(RefCell::new(new_env));
        }
    }
}


impl From<Fun> for Value {
    fn from(f: Fun) -> Self {
        Value::FUN(Box::new(f))
    }
}

