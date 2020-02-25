use std::collections::HashMap;
use crate::value::{Value};
use crate::error::RuntimeError;
use crate::error::RuntimeError::UndefinedProperty;

#[derive(Clone, PartialEq, Debug)]
pub struct LoxClass {
    name: String,
    methods: HashMap<String, Value>
}


impl LoxClass {
    pub fn new(name: String, methods: HashMap<String, Value>) -> Self {
        Self {
            name,
            methods
        }
    }

    pub fn arity(&self) -> i8 {
        0
    }

    pub fn to_string(&self) -> String {
        format!("{} class", self.name)
    }
}


#[derive(Clone, PartialEq, Debug)]
pub struct LoxInstance {
    class: LoxClass,
    fields: HashMap<String, Value>
}

impl LoxInstance {
    pub fn new(class: LoxClass)-> Self {
        Self {
            class,
            fields: HashMap::new()
        }
    }

    pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
        if let Some(v) = self.fields.get(name) {
            Ok(v.clone())
        } else if let Some(m) = self.class.methods.get(name) {
            Ok(m.clone())
        } else {
            Err(UndefinedProperty(name.to_string()))
        }
    }

    pub fn set(&mut self, name: &str, value: &Value) {
        self.fields.insert(name.to_string(), value.clone());
    }

    pub fn to_string(&self) -> String {
        format!("{} instance", self.class.name)
    }
}