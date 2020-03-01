use std::fmt::{Display, Formatter};
use Value::*;
use std::ops::{Neg, Add, Sub, Mul, Div};
use std::cmp::Ordering;

use crate::bytecode::object::{Object, Function, NativeFn};
use crate::bytecode::object::Obj::*;
use std::ptr::NonNull;

#[derive(PartialEq, Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Obj(NonNull<Object>),
    Nil
}



impl Value {
    pub(crate) fn is_number(&self) -> bool {
        if let Value::Number(_) = self {
            true
        } else {
            false
        }
    }
    pub(crate) fn is_false(&self) -> bool {
        match self {
            Nil => true,
            Bool(v) => *v == false,
            _ => false
        }
    }
    pub(crate) fn is_string(&self) -> bool {
        if let Value::Obj(obj) = self {
            return unsafe {
                obj.as_ref().is_string()
            }
        }
        false
    }

    pub(crate) fn as_str(&self) -> &str {
        if let Value::Obj(obj) = self {
            return unsafe {
                obj.as_ref().as_str()
            }
        }
        panic!("not a string")
    }

    pub(crate) fn is_function(&self) -> bool {
        if let Value::Obj(obj) = self {
            return unsafe {
                obj.as_ref().is_function()
            }
        }
        false
    }

    pub(crate) fn as_function(&mut self) -> &mut Function {
        if let Value::Obj(obj) = self {
            return unsafe {
                obj.as_mut().as_function()
            }
        }
        panic!("not a string")
    }

    pub fn is_native(&self) -> bool {
        if let Value::Obj(obj) = self {
            return unsafe {
                if let ObjNative(_) = obj.as_ref().obj {
                    true
                } else {
                    false
                }
            }
        }
        false
    }

    pub fn as_native(&self) -> NativeFn {
        if let Value::Obj(obj) = self {
            unsafe {
                if let ObjNative(n) = obj.as_ref().obj {
                    return n
                }
            }
        }
        panic!("not a native fn");
    }
}


impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            Number(a) => {
                if let Value::Number(b) = other {
                    return a.partial_cmp(b)
                }
                return None;
            },
            Bool(a) => {
                if let Value::Bool(b) = other {
                    a.partial_cmp(b)
                } else {
                    None
                }
            },
            Obj(obj) => {
                if let Value::Obj(o) = other {
                    obj.partial_cmp(o)
                } else {
                    None
                }
            },
            Nil => {
                if let Value::Nil = other {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            },
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}
impl From<f64> for Value {
    fn from(b: f64) -> Self {
        Value::Number(b)
    }
}

impl  From<NonNull<Object>> for Value {
    fn from(o: NonNull<Object>) -> Self {
        Value::Obj(o)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Number(v) => write!(f, "{}", v),
            Bool(b) => write!(f, "{}", b),
            Nil => write!(f, "nil"),
            Obj(obj) => unsafe {
                match obj.as_ref().obj {
                    ObjString(_) => {
                        write!(f, "{}", obj.as_ref().as_str())
                    }
                    ObjFunction(Function{ name, ..}) => {
                        if name.is_null() {
                            write!(f, "<script")
                        } else {
                            write!(f, "<fn {}>", Value::Obj(NonNull::new_unchecked(name)).as_str())
                        }
                    }
                    ObjNative(_) => {
                        write!(f, "<native fn>")
                    }
                    _ => {
                        write!(f, "<obj>")
                    }
                }

            }
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        match self {
            Number(v) => Value::Number(-v),
            _ => {unreachable!()}
        }
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Number(a) => {
                if let Number(b) = rhs {
                    Value::Number(a + b)
                } else {
                    panic!("rhs is not a number!")
                }
            }
            _ => {panic!("lhs is not a number!")}

        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Number(a) => {
                if let Number(b) = rhs {
                    Value::Number(a - b)
                } else {
                    panic!("rhs is not a number!")
                }
            }
            _ => {unreachable!()}

        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Number(a) => {
                if let Number(b) = rhs {
                    Value::Number(a * b)
                } else {
                    panic!("rhs is not a number!")
                }
            }
            _ => {unreachable!()}

        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Number(a) => {
                if let Number(b) = rhs {
                    Value::Number(a / b)
                } else {
                    panic!("rhs is not a number!")
                }
            }
            _ => {unreachable!()}

        }
    }
}