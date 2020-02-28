use std::collections::HashMap;
use std::hash::{Hash, Hasher};


#[derive(Debug, Copy, Clone, Eq)]
struct InternalStrRef(*const str);


pub struct StringTable {
    strings: Vec<Box<str>>,
    map: HashMap<InternalStrRef, usize>
}


impl PartialEq for InternalStrRef {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl InternalStrRef {

    fn from_str(val: &str) -> Self {
        InternalStrRef(val as *const str)
    }

    fn as_str(&self) -> &str {
        unsafe { &*self.0 }
    }
}

impl Hash for InternalStrRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}


impl StringTable {
    pub fn new() -> Self {
        Self {
            strings: Vec::new(),
            map: HashMap::new()
        }
    }

    pub fn intern(&mut self, s: String) -> InternString {
        let id = self.strings.len();
        let boxed_str = s.into_boxed_str();
        let inter_str = InternalStrRef(boxed_str.as_ref() as *const str);
        self.strings.push(boxed_str);
        self.map.insert(inter_str, id);
        id.into()
    }

    pub fn get_or_intern(&mut self, str: &str) -> InternString {
        match self.map.get(&InternalStrRef::from_str(str)) {
            Some(id) => (*id).into(),
            None => self.intern(str.to_string())
        }
    }

    pub fn resolve(&self, InternString(id): &InternString) -> &str {
        unsafe { self.strings.get_unchecked(*id) }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct InternString(pub usize);

impl From<usize> for InternString {
    fn from(id: usize) -> Self {
        InternString(id)
    }
}

