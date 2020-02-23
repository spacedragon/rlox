use std::collections::HashMap;
use crate::error::RuntimeError;
use crate::scanner::Token;
use crate::scanner::TokenType::*;
use crate::error::RuntimeError::{UndefinedVar, ExpectIdentifier};
use crate::value::Value;
use std::time::SystemTime;
use crate::value::callable::Fun;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, PartialEq)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

fn clock(_: Vec<Value>) -> Value {
    let now = SystemTime::now();
    let d = now.duration_since(SystemTime::UNIX_EPOCH)
        .expect("SystemTime before UNIX EPOCH!");
    Value::NUMBER(d.as_secs_f64())
}


impl Environment {
    pub fn global() -> Self {
        let mut values = HashMap::new();
        values.insert(String::from("clock"),
                      Fun::Native(String::from("<fn clock>"),
                                  0,
                                  clock).into());


        Environment {
            values,
            enclosing: None,
        }
    }

    pub fn new(enclsoing: Rc<RefCell<Environment>>) -> Self {
        Self {
            enclosing: Some(enclsoing),
            values: HashMap::new(),
        }
    }

    pub fn get(&self, token: &Token) -> Result<Value, RuntimeError> {
        if let Token { token_type: IDENTIFIER(name), .. } = token {
            if let Some(value) = self.values.get(name) {
                return Ok(value.clone());
            } else if let Some(enclosing) = &self.enclosing {
                return enclosing.borrow().get(token);
            }

            return Err(UndefinedVar { 0: name.clone() });
        }

        Err(ExpectIdentifier)
    }

    pub fn get_at(&self, t: &Token, depth: u32) -> Result<Value, RuntimeError> {
        return if depth == 0 {
            self.get(t)
        } else {
            self.ancestor(depth).borrow().get(t)
        }
    }

    pub fn ancestor(&self, depth: u32) -> Rc<RefCell<Environment>> {
        let mut ret = self.parent();
        for _ in 1..depth {
            let p  = ret.borrow().parent();
            ret = p.clone()
        }
        ret
    }

    pub fn parent(&self) -> Rc<RefCell<Environment>> {
        self.enclosing.as_ref().unwrap().clone()
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn assign_at(&mut self, t: &Token, value: &Value, depth: u32) -> Result<(), RuntimeError> {
        return if depth == 0 {
            self.assign(t, value)
        } else {
            self.ancestor(depth).borrow_mut().assign(t, value)
        }
    }

    pub fn assign(&mut self, token: &Token, value: &Value) -> Result<(), RuntimeError> {
        if let Token { token_type: IDENTIFIER(name), .. } = token {
            if self.values.contains_key(name) {
                self.values.insert(name.clone(), value.clone());
                return Ok(());
            } else if self.enclosing.is_some() {
                let enclosing = self.enclosing.as_ref().unwrap();
                return enclosing.borrow_mut().assign(token, value);
            }

            return Err(UndefinedVar(name.clone()));
        }
        panic!("left is not a target");
    }
}