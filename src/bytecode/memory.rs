use std::ptr;
use std::mem;
use std::collections::HashMap;
use super::object::Obj;
use Obj::*;
use std::ptr::NonNull;
use std::cell::RefCell;
use super::object::Object;
use crate::bytecode::chunk::Chunk;
use crate::bytecode::object::{Function, NativeFn, Closure};
use crate::bytecode::value::Value;


pub struct Allocator {
    objects: *mut Object,
    strings: HashMap<Box<str>,NonNull<Object>>
}

impl Allocator {

    pub fn new() -> Self {
        Self {
            objects: ptr::null_mut(),
            strings: HashMap::new(),
        }
    }

    fn allocate(&mut self, obj: Obj) -> NonNull<Object> {
        let object = Box::new(Object {
            obj,
            next: self.objects
        });
        let ptr = Box::into_raw(object);
        self.objects = ptr;
        unsafe { NonNull::new_unchecked(ptr) }
    }

    pub fn allocate_string(&mut self, chars: &[char]) -> NonNull<Object> {
        let str: String = chars.iter().collect();
        self.copy_string(str)
    }

    pub fn copy_string(&mut self, str: String) -> NonNull<Object> {
        let string = str.into_boxed_str();
        if let Some(object) = self.strings.get(&string) {
            NonNull::from(object.clone())
        } else {
            let str_ptr = string.as_ref() as *const str;
            let object = self.allocate(ObjString(str_ptr));
            self.strings.insert(string, object);
            object
        }
    }

    pub fn free_objects(&mut self) {
        let mut object = self.objects;
        while !object.is_null() {
            let next = unsafe { (*object).next };
            self.free_object(object);
            object = next;
        }
        self.objects = object;
    }


    pub fn free_object(&mut self, ptr: *mut Object) {
        let object = unsafe { Box::from_raw(ptr)} ;
        match object.obj {
            ObjString(str_ptr) => {
                let string = unsafe { Box::from_raw(str_ptr as *mut str)};
                self.strings.remove(&string);
                mem::forget(string);
            },
            ObjFunction(Function { ..}) => {
                //self.free_object(name);
            }
            ObjNative(_) => {}
            ObjClosure(..) => {}
            ObjUpvalue(..) => {}
        }
    }

    pub fn new_function(&mut self) -> NonNull<Object> {
        let function = Function {
            arity: 0,
            name: ptr::null_mut(),
            chunk: Chunk::new(),
            upvalue_count: 0
        };
        self.allocate(ObjFunction(function))
    }

    pub fn new_closure(&mut self, function: NonNull<Object>) -> NonNull<Object> {
        unsafe {
            match function.as_ref().obj {
                ObjFunction(ref f) => {
                    self.allocate(ObjClosure(Closure {
                        function,
                        upvalues: vec![ NonNull::dangling() ; f.upvalue_count as usize]
                    }))
                },
                _ => {
                    panic!("can't convert to closure")
                }
            }
        }

    }

    pub fn new_native(&mut self, function: NativeFn) -> NonNull<Object>{
        self.allocate(Obj::ObjNative(function))
    }

    pub fn new_upvalue(&mut self, slot: NonNull<Value>) -> NonNull<Object>{
        self.allocate(Obj::ObjUpvalue(slot))
    }
}

pub fn new_closure(function: NonNull<Object>) -> NonNull<Object> {
    ALLOCATOR.with(|a| {
        a.borrow_mut().new_closure(function)
    })
}

pub fn new_native(function: NativeFn) -> NonNull<Object>{
    ALLOCATOR.with(|a| {
        a.borrow_mut().new_native(function)
    })
}

pub fn new_upvalue(slot: &Value) -> NonNull<Object> {
    ALLOCATOR.with(|a| {
        a.borrow_mut().new_upvalue(NonNull::from(slot))
    })
}

impl Drop for Allocator {
    fn drop(&mut self) {
        self.free_objects();
    }
}

thread_local!(pub static ALLOCATOR: RefCell<Allocator> = RefCell::new(Allocator::new()));

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_allocate() {
        let mut aa = Allocator::new();
        let chars: Vec<char> = "ccc".chars().collect();
        let object = aa.allocate_string(&chars);
        unsafe {
            assert!(object.as_ref().is_string());
            assert_eq!(object.as_ref().as_str(), "ccc");
        }
    }

    #[test]
    fn test_free() {
        let mut aa = Allocator::new();
        let chars: Vec<char> = "ccc".chars().collect();
        let _object = aa.allocate_string(&chars);
        aa.free_objects();
        assert!(aa.strings.is_empty());
        assert!(aa.objects.is_null());
    }
}

