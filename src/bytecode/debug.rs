use super::chunk::Chunk;
use super::OpCode;
use super::OpCode::*;
use std::convert::TryFrom;

#[cfg(test)]
pub mod output {
    use std::cell::RefCell;
    thread_local!(static STDERR: RefCell<String> = RefCell::new(String::new()));
    thread_local!(static STDOUT: RefCell<String> = RefCell::new(String::new()));

    pub fn stderr_string() -> String {
        STDERR.with(|out| {
            out.borrow().to_string()
        })
    }

    pub fn stderr(s: String) {
        STDERR.with(|out| {
            out.borrow_mut().push_str(&s)
        })
    }
    
    pub fn stdout(s: String) {
        STDOUT.with(|out| {
            out.borrow_mut().push_str(&s)
        })
    }

    pub fn stdout_string() -> String {
        STDOUT.with(|out| {
            out.borrow().to_string()
        })
    }
}

#[cfg(not(test))]
pub mod output {
    use std::io::{Write};

    pub fn stdout(s: String) {
        let stdout1 = std::io::stdout();
        let mut out = stdout1.lock();
        out.write_all(s.as_bytes()).expect("");
    }

    pub fn stderr(s: String) {
        let stdout1 = std::io::stderr();
        let mut out = stdout1.lock();
        out.write_all(s.as_bytes()).expect("");
    }
}



use output::{stdout, stderr};


pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    stderr(format!("=={} chunk==\n", name));

    let mut offset = 0;
    loop {
        offset = disassemble_instruction(chunk, offset);
        if offset >= chunk.len() {
            break;
        }
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    stderr(format!("{:04} ", offset));
    if offset > 0 && chunk.line(offset) == chunk.line(offset - 1) {
        stderr(format!("   | "));
    } else {
        stderr(format!("{:4} ", chunk.line(offset)));
    }

    let instruction = chunk[offset];

    if let Ok(op) = OpCode::try_from(instruction) {
        match op {
            OpReturn => {
                simple_instruction("OP_RETURN", offset)
            }
            OpConstant => {
                constant_instruction("OP_CONSTANT", chunk, offset)
            }
            OpConstantLong => {
                constant_instruction_long("OP_CONSTANT_LONG", chunk, offset)
            }
            OpNegate => {
                simple_instruction("OP_NEGATE", offset)
            }
            OpAdd => simple_instruction("OP_ADD", offset),
            OpSubtract => simple_instruction("OP_SUBTRACT", offset),
            OpMultiply => simple_instruction("OP_MULTIPLY", offset),
            OpDivide =>  simple_instruction("OP_DIVIDE", offset),
            OpNil => simple_instruction("OP_NIL", offset),
            OpTrue => simple_instruction("OP_TRUE", offset),
            OpFalse => simple_instruction("OP_FALSE", offset),
            OpNot => simple_instruction("OP_NOT", offset),
            OpEqual => simple_instruction("OP_EQUAL", offset),
            OpGreater => simple_instruction("OP_GREATER", offset),
            OpLess => simple_instruction("OP_LESS", offset),
            OpPrint => simple_instruction("OP_PRINT", offset),
            OpPop => simple_instruction("OP_POP", offset),
            OpDefineGlobal => constant_instruction("OP_DEFINE_GLOBAL", chunk, offset),
            OpGetGlobal => constant_instruction("OP_GET_GLOBAL", chunk, offset),
            OpSetGlobal => constant_instruction("OP_SET_GLOBAL", chunk, offset),
            OpPopN => byte_instruction("OP_POPN", chunk,offset) ,
            OpGetLocal => byte_instruction("OP_GET_LOCAL", chunk,offset),
            OpSetLocal => byte_instruction("OP_SET_LOCAL", chunk,offset),
            OpJump => jump_instruction("OP_JUMP", 1, chunk, offset),
            OpJumpIfFalse => jump_instruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
            OpLoop => jump_instruction("OP_LOOP", -1, chunk, offset),
            OpCall => byte_instruction("OP_CALL", chunk,offset) ,
            OpClosure => {
                let mut offset = offset + 1;
                let constant = chunk[offset];
                offset +=1;
                stderr(format!("{:16} {:4} {}\n", "OP_CLOSURE", constant,
                               chunk.constant(constant as usize)));
                offset
            }
        }
    } else {
        stderr(format!("Unknown opcode {}\n", instruction));
        offset + 1
    }
}


fn simple_instruction(name: &str, offset: usize) -> usize {
    stderr(format!("{:16}:\n", name));
    offset + 1
}

fn jump_instruction(name: &str,sign: i32, chunk: &Chunk, offset: usize) -> usize {
    let jump = u16::from_le_bytes([chunk[offset+1],chunk[offset+2]]);
    let j: i32 = offset as i32 + 3 + sign * (jump as i32);
    stderr(format!("{:16} {:4} -> {}:\n", name, offset, j));
    offset + 3
}

fn byte_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk[offset + 1];
    stderr(format!("{:16}: {:04} \n", name, slot));
    offset + 2
}
fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = chunk[offset + 1];
    stderr(format!("{:16}: {:04} '{}'\n", name, constant,
                   chunk.constant(constant as usize)));
    offset + 2
}

fn constant_instruction_long(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = u16::from_be_bytes([chunk[offset + 1], chunk[offset + 2]]);
    stderr(format!("{:16}: {:04} '{}'\n", name, constant,
                    chunk.constant(constant as usize)));

    offset + 3
}

pub fn print_err(str: String) {
    stderr(str);
}

pub fn print(str: String) {
    stdout(str);
}

pub fn println(str: String) {
    stdout(str);
    stdout("\n".to_string());
}
 
