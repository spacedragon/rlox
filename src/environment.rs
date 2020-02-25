use std::collections::HashMap;
use crate::error::RuntimeError;
use crate::error::RuntimeError::{UndefinedVar};
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

    pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
        if let Some(value) = self.values.get(name) {
            return Ok(value.clone());
        } else if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().get(name);
        }

        return Err(UndefinedVar { 0: name.to_string() });
    }

    pub fn get_at(&self, t: &str, depth: u32) -> Result<Value, RuntimeError> {
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

    pub fn assign_at(&mut self, name: &str, value: &Value, depth: u32) -> Result<(), RuntimeError> {
        return if depth == 0 {
            self.assign(name, value)
        } else {
            self.ancestor(depth).borrow_mut().assign(name, value)
        }
    }

    pub fn assign(&mut self, name: &str, value: &Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value.clone());
            return Ok(());
        } else if self.enclosing.is_some() {
            let enclosing = self.enclosing.as_ref().unwrap();
            return enclosing.borrow_mut().assign(name, value);
        }

        return Err(UndefinedVar(name.to_string()));
    }
}