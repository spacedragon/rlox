use std::collections::HashMap;
use crate::interpreter::RuntimeError;
use crate::scanner::Token;
use crate::scanner::TokenType::*;
use crate::interpreter::RuntimeError::{UndefinedVar, ExpectIdentifier};
use crate::value::{Value};
use std::time::SystemTime;
use crate::value::callable::Fun;
use std::rc::Rc;
use failure::_core::cell::RefCell;

#[derive(Debug, Clone, PartialEq)]
pub struct  Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>
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
            enclosing: None
        }
    }

    pub fn new(enclsoing: Rc<RefCell<Environment>>)-> Self {
        Self {
            enclosing: Some(enclsoing),
            values: HashMap::new()
        }
    }

    pub fn get(&self, token: &Token) -> Result<Value, RuntimeError>{
        if let Token{ token_type: IDENTIFIER(name) , .. } = token {

            if let Some(value) = self.values.get(name) {
                return Ok(value.clone());
            } else if let Some(enclosing)= &self.enclosing {
                return enclosing.borrow().get(token)
            }

            return Err(UndefinedVar { name: name.clone() });
        }

        Err(ExpectIdentifier)
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);

    }

    pub fn assign(&mut self, token: &Token, value: &Value) -> Result <(), RuntimeError> {
        if let Token{ token_type: IDENTIFIER(name) , .. } = token {
            if self.values.contains_key(name) {
                    self.values.insert(name.clone(), value.clone());
                    return Ok(())
            } else if  self.enclosing.is_some() {
                let mut enclosing = self.enclosing.as_ref().unwrap();
                return enclosing.borrow_mut().assign(token, value);
            }

            return Err(UndefinedVar { name: name.clone() })
        }
        panic!("left is not a target");
    }


}