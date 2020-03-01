use std::collections::HashMap;
use crate::value::{Value, Fun};
use crate::error::RuntimeError;
use crate::error::RuntimeError::UndefinedProperty;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};

type PClass = Rc<RefCell<LoxClass>>;

#[derive(Clone, PartialEq, Debug)]
pub struct LoxClass {
    name: String,
    methods: HashMap<String, Value>,
    superclass: Option<PClass>,
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} class", self.name)
    }
}

impl LoxClass {
    pub fn new(name: String, methods: HashMap<String, Value>, superclass: Option<PClass>) -> Self {
        Self {
            name,
            methods,
            superclass,
        }
    }

    pub fn arity(&self) -> u8 {
        if let Some(fun) = self.find_method("init") {
            fun.arity()
        } else {
            0
        }
    }

    pub fn find_method(&self, name: &str) -> Option<Box<Fun>> {
        if let Some(Value::FUN(fun)) = self.methods.get(name) {
            let f = fun.clone();
            Some(f)
        } else if let Some(s) = &self.superclass {
            s.borrow().find_method(name)
        } else {
            None
        }
    }
}


#[derive(Clone, PartialEq, Debug)]
pub struct LoxInstance {
    class: PClass,
    fields: HashMap<String, Value>,
}

impl LoxInstance {
    pub fn new(class: PClass) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
        if let Some(v) = self.fields.get(name) {
            Ok(v.clone())
        } else if let Some(m) = self.class.borrow().find_method(name) {
            Ok(Value::FUN(m))
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