use std::collections::HashMap;
use crate::value::{Value, Fun};
use crate::error::RuntimeError;
use crate::error::RuntimeError::UndefinedProperty;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};

#[derive(Clone, PartialEq, Debug)]
pub struct LoxClass {
    name: String,
    methods: HashMap<String, Value>
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} class", self.name)
    }
}

impl LoxClass {
    pub fn new(name: String, methods: HashMap<String, Value>) -> Self {
        Self {
            name,
            methods
        }
    }

    pub fn arity(&self) -> i8 {
        if let Some(fun) = self.find_method("init") {
            fun.arity()
        } else {
            0
        }
    }

    pub fn find_method(&self, name: &str) -> Option<&Fun> {
        if let Some(Value::FUN(fun)) = self.methods.get(name) {
            Some(fun)
        } else {
            None
        }
    }

    pub fn find_method_mut(&mut self, name: &str) -> Option<&mut Fun> {
        if let Some(Value::FUN(fun)) = self.methods.get_mut(name) {
            Some(fun)
        } else {
            None
        }
    }
}


#[derive(Clone, PartialEq, Debug)]
pub struct LoxInstance {
    class: Rc<RefCell<LoxClass>>,
    fields: HashMap<String, Value>
}

impl LoxInstance {
    pub fn new(class: Rc<RefCell<LoxClass>>)-> Self {
        Self {
            class,
            fields: HashMap::new()
        }
    }

    pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
        if let Some(v) = self.fields.get(name) {
            Ok(v.clone())
        } else if let Some(m) = self.class.borrow().methods.get(name) {
            Ok(m.clone())
        } else {
            Err(UndefinedProperty(name.to_string()))
        }
    }

    pub fn set(&mut self, name: &str, value: &Value) {
        self.fields.insert(name.to_string(), value.clone());
    }

}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class.borrow().name)
    }
}