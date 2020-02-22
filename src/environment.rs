use std::collections::{HashMap, LinkedList};
use crate::interpreter::{Value, RuntimeError};
use crate::scanner::Token;
use crate::scanner::TokenType::*;
use crate::interpreter::RuntimeError::{UndefinedVar, ExpectIdentifier};

pub struct  Environment {
    scopes: LinkedList<HashMap<String, Value>>
}



impl Environment {
    pub fn new() -> Self {
        let mut scopes = LinkedList::new();
        scopes.push_back(HashMap::new());
        Environment {
            scopes
        }
    }

    pub fn get(&self, token: &Token) -> Result<Value, RuntimeError>{
        if let Token{ token_type: IDENTIFIER(name) , .. } = token {
            for scope in &self.scopes {
                if let Some(value) = scope.get(name) {
                    return Ok(value.clone());
                }
            }
            return Err(UndefinedVar { name: name.clone() });
        }

        Err(ExpectIdentifier)
    }

    pub fn define(&mut self, name: String, value: Value) {
        if let Some(scope) = self.scopes.front_mut() {
            scope.insert(name, value);
        };
    }

    pub fn assign(&mut self, token: &Token, value: &Value) -> Result <(), RuntimeError> {
        if let Token{ token_type: IDENTIFIER(name) , .. } = token {
            for scope in &mut self.scopes {
                if scope.contains_key(name) {
                    scope.insert(name.clone(), value.clone());
                    return Ok(())
                }
            }
            return Err(UndefinedVar { name: name.clone() })
        }
        panic!("left is not a target");
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop_front();
    }

    pub fn new_scope(&mut self) {
        self.scopes.push_front(HashMap::new());
    }
}