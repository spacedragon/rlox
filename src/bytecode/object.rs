use Obj::*;
use crate::bytecode::chunk::Chunk;

pub enum Obj {
    ObjString(*const str),
    ObjFunction(Function)
}

pub struct Function {
    pub arity: usize,
    pub(crate) chunk: Chunk,
    pub name: *mut Object
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

    pub fn as_function(&mut self) -> &mut Function {
        match self.obj {
            ObjFunction(ref mut ptr) => {
                ptr
            }
            _ => panic!("not a function!"),
        }
    }
}


