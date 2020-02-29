
use std::ops::{Index, IndexMut};
use super::value::Value;
use super::OpCode;
use crate::bytecode::OpCode::{OpConstant, OpConstantLong};
use crate::bytecode::string_table::{StringTable, InternString};


pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<i32>,
    strings: StringTable,
}


impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants:Vec::new(),
            lines: Vec::new(),
            strings: StringTable::new()
        }
    }

    pub fn write_chunk(&mut self, byte: u8, line: i32) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn write_op(&mut self, op: OpCode, line: i32) {
        self.write_chunk(op.into(), line)
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn get(&self, offset: usize) -> Option<u8> {
        self.code.get(offset).copied()
    }

    pub(crate) fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1) as u8
    }

    pub fn write_constant(&mut self, value: Value, line: i32) {
        self.constants.push(value);
        let idx = self.constants.len() -1;
        if idx < 256 {
            self.write_op(OpConstant, line);
            self.write_chunk(idx as u8, line);
        } else {
            self.write_op(OpConstantLong, line);
            let [h, l] = (idx as u16).to_le_bytes();
            self.write_chunk(h, line);
            self.write_chunk(l, line);
        }
    }

    pub fn constant(&self, offset: usize) -> &Value {
        &self.constants[offset]
    }

    pub fn line(&self, offset: usize) -> i32 {
        self.lines[offset]
    }

    pub fn make_string(&mut self, s: String) -> InternString {
        self.strings.get_or_intern(&s)
    }
    pub fn get_string(&self, id: &InternString) -> &str {
        self.strings.resolve(id)
    }
}

impl Index<usize> for Chunk {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        unsafe  { self.code.get_unchecked(index) }
    }
}

impl IndexMut<usize> for Chunk {
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        unsafe  { self.code.get_unchecked_mut(idx) }
    }
}