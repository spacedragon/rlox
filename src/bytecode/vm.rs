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
use std::collections::HashMap;
use crate::bytecode::memory::{ALLOCATOR};
use crate::bytecode::compiler::Compiler;
use crate::bytecode::object::NativeFn;
use std::time::SystemTime;

const STACK_MAX: usize = 256;
const FRAMES_MAX: usize = 64;

struct CallFrame {
    function: Value,
    ip: usize,
    slot_offset: usize
}

pub struct VM {
    frames: Vec<CallFrame>,
    globals: HashMap<String, Value>,
    stack: Vec<Value>
}

impl VM {
    pub fn new() -> Self {
        Self {
            frames: Vec::with_capacity(FRAMES_MAX),
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub fn init(&mut self) {
        self.reset_stack();
        self.define_native("clock", clock);
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

    fn current_chunk(&mut self)-> &Chunk {
        &self.current_frame().function.as_function().chunk
    }

    fn current_frame(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    pub fn interpret(&mut self, source: &str) {
        self.init();
        let compiler = Compiler::new(source);
        match compiler.compile() {
            Ok(function) => {
                self.push(function.clone());
                if let Err(e) = self.call_value(function, 0) {
                    self.runtime_err(e);
                } else {
                    if let Err(e) = self.run() {
                        self.runtime_err(e)
                    }
                }
            }
            Err(err) => {
                print_err(format!("{}", err))
            }
        }
    }

    fn runtime_err(&mut self, e: RuntimeError) {
        print_err(format!("{}\n", e));
        for frame in self.frames.iter_mut().rev() {
            let ip = frame.ip;
            let function = frame.function.as_function();
            let chunk = &function.chunk;
            let line = chunk.line(ip);
            let name = if function.name.is_null() {
                "script"
            } else {
                function.name()
            };
            print_err(format!("{} [line {} in {}]", e, line, name))
        }

    }

    fn read_byte(&mut self) -> u8 {
        let ip = self.current_frame().ip;
        let b = self.current_chunk()[ip];
        self.current_frame().ip += 1;
        b
    }

    fn read_u16(&mut self) -> u16 {
        let ip = self.current_frame().ip;
        let chunk = self.current_chunk();
        let v = u16::from_le_bytes([chunk[ip], chunk[ip + 1]]);
        self.current_frame().ip += 2;
        v
    }

    fn read_constant(&mut self) -> Value {
        let b = self.read_byte();
        self.current_chunk().constant(b as usize).clone()
    }

    fn read_constant_long(&mut self) -> Value {
        let b = self.read_u16();
        self.current_chunk().constant(b as usize).clone()
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        loop {

            #[cfg(debug_assertions)]
                {
                    print_err("stack [".to_string());
                    for v in &self.stack {
                        print_err(format!("{},", v))
                    }
                    print_err("]\n".to_string());
                    let ip = self.current_frame().ip;
                    disassemble_instruction(&self.current_chunk(), ip);
                }

            let instruction = self.read_byte();
            if let Ok(op) = OpCode::try_from(instruction) {
                match op {
                    OpReturn => {
                        let result = self.pop();
                        if let Some(frame) = self.frames.pop() {
                            if self.frames.is_empty() {
                                self.pop();
                                return Ok(())
                            }

                            self.stack.truncate(frame.slot_offset);
                            self.push(result);
                        }
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
                            let s = format!("{}{}", a.as_str(),
                                            b.as_str());
                            let chars: Vec<char> = s.chars().collect();
                            let c = ALLOCATOR.with(|a|{
                                a.borrow_mut().allocate_string(&chars)
                            });
                            self.push(Value::Obj(c))
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
                        let v = self.pop();
                        self.print_value(v);
                    }
                    OpPop => { self.pop(); }
                    OpPopN => {
                        let n = self.read_byte() as usize;
                        self.stack.truncate(self.stack.len() - n);
                    }
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
                            return Err(UndefinedVar(name));
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
                    OpGetLocal => {
                        let slot = self.read_byte() as usize;
                        let frame = self.current_frame();
                        let frame_slot = frame.slot_offset + slot;
                        let value = self.stack[frame_slot].clone();
                        self.push(value);
                    }
                    OpSetLocal => {
                        let slot = self.read_byte() as usize;
                        let frame_slot = self.current_frame().slot_offset + slot;
                        self.stack[frame_slot] = self.peek(0).clone();
                    }
                    OpJumpIfFalse => {
                        let offset = self.read_u16();
                        if self.peek(0).is_false() {
                            self.current_frame().ip += offset as usize;
                        }
                    }
                    OpJump => {
                        let offset = self.read_u16();
                        self.current_frame().ip += offset as usize;
                    }
                    OpLoop => {
                        let offset = self.read_u16();
                        self.current_frame().ip -= offset as usize;
                    }
                    OpCall => {
                        let arg_count = self.read_byte();
                        let callee = self.peek(arg_count as usize).clone();
                        self.call_value(callee, arg_count)?;
                    }
                }
            } else {}
        }
    }

    fn call_value(&mut self, callee: Value, arg_count: u8) -> Result<(), RuntimeError> {
        match callee {
            f if f.is_function() => {
                self.call(f, arg_count)
            }
            n if n.is_native() => {
                let native = n.as_native();
                let new_len = self.stack.len() - (arg_count as usize) - 1;
                let args = self.stack.split_off(new_len);
                let result = native(args);
                self.push(result);
                Ok(())
            }
            _ => {
                Err(NotCallable)
            }
        }
    }

    fn call(&mut self, mut function: Value, arg_count: u8) -> Result<(), RuntimeError> {
        let arity = function.as_function().arity as u8;
        if arg_count != arity as u8 {
            return Err(ArgumentsSizeNotMatch(arity, arg_count));
        }
        let frame = CallFrame {
            function,
            ip: 0,
            slot_offset: self.stack.len() - (arg_count as usize) - 1,
        };
        self.frames.push(frame);
        Ok(())
    }

    fn read_string(&mut self) -> String {
        let c = self.read_constant();
        let name = c.as_str();
        name.to_string()
    }

    fn print_value(&self, v: Value) {
        println(format!("{}", v))
    }

    fn define_native(&mut self, name: &str, function: NativeFn) {
        ALLOCATOR.with(|a| {
            let mut alloc = a.borrow_mut();
            let fn_name = Value::Obj(alloc.copy_string(name.to_string()));
            self.push(fn_name);
            let native = Value::Obj(alloc.new_native(function));
            self.push(native.clone());
            self.globals.insert(name.to_string(), native);
            self.pop();
            self.pop();
        })
    }

}



pub fn clock(_: Vec<Value>) -> Value {
    let now = SystemTime::now();
    let d = now.duration_since(SystemTime::UNIX_EPOCH)
        .expect("SystemTime before UNIX EPOCH!");
    Value::Number(d.as_secs_f64())
}

