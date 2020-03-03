use Obj::*;
use crate::bytecode::chunk::Chunk;
use crate::bytecode::value::Value;
use std::ptr::NonNull;

pub enum Obj {
    ObjString(*const str),
    ObjFunction(Function),
    ObjNative(NativeFn),
    ObjClosure(Closure),
    ObjUpvalue(NonNull<Value>),
}

pub type NativeFn = fn(args: Vec<Value>) -> Value;

pub struct Function {
    pub arity: usize,
    pub(crate) chunk: Chunk,
    pub name: *mut Object,
    pub upvalue_count: u8,
}

pub struct Closure {
    pub function: NonNull<Object>,
    pub upvalues: Vec<NonNull<Object>>
}

impl Function {
    pub fn name(&self) ->&str {
        unsafe {
            self.name.as_ref().map(|n| n.as_str()).unwrap_or("script")
        }
    }
}

pub struct Object {
    pub obj: Obj,
    pub(crate) next: *mut Object
}


impl Object {
    pub fn is_string(&self) -> bool {
        match self.obj {
            ObjString(_) => true,
            _ => false,
        }
    }
    pub fn as_str(&self) -> &str{
        match self.obj {
            ObjString(ptr) => unsafe {
                &*ptr
            }
            _ => panic!("not a string!"),
        }
    }

    pub fn is_function(&self) -> bool {
        match self.obj {
            ObjFunction{..} => true,
            _ => false,
        }
    }

    pub fn as_function(&self) -> &Function {
        match self.obj {
            ObjFunction(ref ptr) => {
                ptr
            }
            ObjClosure(ref c) => unsafe {
                if let ObjFunction(ref f) = c.function.as_ref().obj {
                    return f;
                }
                unreachable!()
            }
            _ => panic!("not a function or closure!"),
        }
    }

    pub fn as_function_mut(&mut self) -> &mut Function {
        match self.obj {
            ObjFunction(ref mut ptr) => {
                ptr
            }
            ObjClosure(ref mut c) => unsafe {
                if let ObjFunction(ref mut f) = c.function.as_mut().obj {
                    return f;
                }
                unreachable!()
            }
            _ => panic!("not a function or closure!"),
        }
    }
}


