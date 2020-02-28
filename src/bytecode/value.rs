use std::fmt::{Display, Formatter};

#[derive(PartialEq, Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Obj(Obj),
    Nil
}

#[derive(Clone, PartialEq)]
pub enum Obj {
    ObjString(InternString)
}



use Value::*;
use std::ops::{Neg, Add, Sub, Mul, Div};
use std::cmp::Ordering;
use crate::bytecode::value::Obj::ObjString;
use crate::bytecode::string_table::InternString;


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
            Bool(v) => !*v,
            _ => false
        }
    }
    pub(crate) fn is_string(&self) -> bool {
        if let Value::Obj(obj) = self {
            if let Obj::ObjString(_) = obj {
                return true
            }
        }
        false
    }

    pub(crate) fn as_string(&self) -> &InternString {
        if let Value::Obj(obj) = self {
            if let Obj::ObjString(s) = obj {
                return s
            }
        }
        panic!("not a string")
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
            Obj(_) => {None},
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

impl From<InternString> for Value {
    fn from(is: InternString) -> Self {
        Value::Obj(ObjString(is))
    }
}


impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Number(v) => write!(f, "{}", v),
            Bool(b) => write!(f, "{}", b),
            Nil => write!(f, "nil"),
            Obj(obj) => {
                match obj {
                    Obj::ObjString(InternString(id)) => {
                        write!(f, "<str {}>", id)
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