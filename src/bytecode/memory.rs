use std::ptr;
use std::mem;
use std::collections::HashMap;
use crate::bytecode::memory::Obj::ObjString;
use std::ptr::NonNull;
use std::cell::RefCell;

pub enum Obj {
    ObjString(*const str)
}

pub struct Object {
    pub obj: Obj,
    next: *mut Object
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
}

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
        NonNull::new(ptr).unwrap()
    }

    pub fn allocate_string(&mut self, chars: &[char]) -> NonNull<Object> {
        let str: String = chars.iter().collect();
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
        }
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
        let object = aa.allocate_string(&chars);
        aa.free_objects();
        assert!(aa.strings.is_empty());
        assert!(aa.objects.is_null());
    }
}

