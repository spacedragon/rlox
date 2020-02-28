macro_rules! expr {
    ($e:expr) => {
        $e
    }
}

macro_rules! bin_op {
    ($s:expr, $op:tt) => {
        if $s.peek(0).is_number() && $s.peek(1).is_number() {
            let b = $s.pop();
            let a = $s.pop();
            $s.push(expr!(a $op b).into());
        } else {
                return Err(OperandMustNumber)
            }
        }
}

use super::chunk::Chunk;
use crate::error::RuntimeError;
use super::OpCode;
use std::convert::TryFrom;
use OpCode::*;
use super::value::Value;
use crate::bytecode::debug::{print_err, disassemble_instruction, println};
use crate::error::RuntimeError::*;
use crate::bytecode::value::Obj::ObjString;
use std::collections::HashMap;

const STACK_MAX: usize = 256;

pub struct VM {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    globals: HashMap<String, Value>
}

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Vec::with_capacity(STACK_MAX),
            globals: HashMap::new()
        }
    }

    pub fn init(&mut self) {
        self.reset_stack();
    }

    fn reset_stack(&mut self) {
        self.stack.clear()
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn peek(&self, d: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - d]
    }

    pub fn interpret(&mut self) {
        if let Err(e) = self.run() {
            let line = self.chunk.line(self.ip);
            print_err(format!("{} [line {} in script]", e, line))
        }
    }

    fn read_byte(&mut self) -> u8 {
        let b = self.chunk[self.ip];
        self.ip += 1;
        b
    }

    fn read_u16(&mut self) -> u16 {
        let v = u16::from_be_bytes([self.chunk[self.ip], self.chunk[self.ip + 1]]);
        self.ip += 2;
        v
    }

    fn read_constant(&mut self) -> Value {
        let b = self.read_byte();
        self.chunk.constant(b as usize).clone()
    }

    fn read_constant_long(&mut self) -> Value {
        let b = self.read_u16();
        self.chunk.constant(b as usize).clone()
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            let instruction = self.read_byte();

            #[cfg(debug_assertions)]
                {
                    print_err("stack [".to_string());
                    for v in &self.stack {
                        print_err(format!("{},", v))
                    }
                    print_err("]\n".to_string());
                    disassemble_instruction(&self.chunk, self.ip - 1);
                }

            if let Ok(op) = OpCode::try_from(instruction) {
                match op {
                    OpReturn => {
                        return Ok(());
                    }
                    OpConstant => {
                        let constant = self.read_constant();
                        self.push(constant);
                    }
                    OpConstantLong => {
                        let constant = self.read_constant_long();
                        self.push(constant);
                    }
                    OpNegate => {
                        if self.peek(0).is_number() {
                            let value = -self.pop();
                            self.push(value)
                        } else {
                            return Err(OperandMustNumber);
                        }
                    }
                    OpAdd => {
                        if self.peek(0).is_string() || self.peek(1).is_string() {
                            let b = self.pop();
                            let a = self.pop();
                            let s = format!("{}{}", self.chunk.get_string(a.as_string()),
                                            self.chunk.get_string(b.as_string()));
                            let c = self.chunk.make_string(s).into();
                            self.push(c)
                        } else {
                            bin_op!(self, +);
                        }
                    }
                    OpSubtract => {
                        bin_op!(self, -);
                    }
                    OpMultiply => {
                        bin_op!(self, *);
                    }
                    OpDivide => {
                        bin_op!(self, /);
                    }
                    OpNil => self.push(Value::Nil),
                    OpTrue => self.push(Value::Bool(true)),
                    OpFalse => self.push(Value::Bool(false)),
                    OpNot => {
                        let v = self.pop();
                        self.push(Value::Bool(v.is_false()))
                    }
                    OpEqual => {
                        let a = self.pop();
                        let b = self.pop();
                        self.push(Value::Bool(a == b));
                    }
                    OpGreater => bin_op!(self, >),
                    OpLess => bin_op!(self, <),
                    OpPrint => {
                        let v= self.pop();
                        self.print_value(v);
                    }
                    OpPop => { self.pop(); }
                    OpDefineGlobal => {
                        let name = self.read_string();
                        let v = self.pop();
                        self.globals.insert(name, v);
                    }
                    OpGetGlobal => {
                        let name = self.read_string();
                        if let Some(v) = self.globals.get(&name) {
                            let v = v.clone();
                            self.push(v)
                        } else {
                            return Err(UndefinedVar(name))
                        }
                    }
                    OpSetGlobal => {
                        let name = self.read_string();
                        if self.globals.contains_key(&name) {
                            self.globals.insert(name, self.peek(0).clone());
                        } else {
                            return Err(UndefinedVar(name));
                        }
                    }
                }
            } else {}
        }
    }


    fn read_string(&mut self) -> String {
        let c = self.read_constant();
        let name = c.as_string();
        self.chunk.get_string(name).to_string()
    }

    fn print_value(&self, v: Value) {
        match v {
            Value::Obj(ObjString(str)) => {
                println(self.chunk.get_string(&str).to_string());
            }
            _ => {
                println(format!("{}", v))
            }
        }
    }
}




