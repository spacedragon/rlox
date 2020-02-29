mod debug;
mod value;
mod memory;
pub(crate) mod scanner;
pub mod chunk;
pub mod vm;
pub mod compiler;
use num_enum::{IntoPrimitive, TryFromPrimitive};

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
pub enum OpCode {
    OpReturn = 1,
    OpConstant,
    OpConstantLong,
    OpNegate,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNil,
    OpTrue,
    OpFalse,
    OpNot,
    OpEqual ,
    OpGreater,
    OpLess,
    OpPrint,
    OpPop,
    OpPopN,
    OpGetGlobal,
    OpSetGlobal,
    OpDefineGlobal,
    OpGetLocal,
    OpSetLocal,
    OpJump,
    OpJumpIfFalse,
    OpLoop
}


#[cfg(test)]
mod test {
    use super::*;
    use super::value::Value;
    use super::chunk::Chunk;
    use OpCode::*;
    use crate::bytecode::vm::VM;
    use super::debug::output::{stderr_string, stdout_string};
    use crate::bytecode::debug::disassemble_chunk;
    use crate::bytecode::scanner::Scanner;
    use crate::bytecode::compiler::Compiler;
    use crate::error::LoxError;

    #[test]
    fn test() {
        let mut chunk = Chunk::new();


        chunk.write_constant(Value::Number(1.2), 123);

        chunk.write_op(OpCode::OpReturn, 123);
        disassemble_chunk(&chunk, "test");

        assert_eq!(stderr_string(), r#"==test chunk==
0000  123 OP_CONSTANT     : 0000 '1.2'
0002    | OP_RETURN       :
"#);
    }

    #[test]
    fn test_simple() {
        let mut chunk = Chunk::new();

        chunk.write_constant(Value::Number(1.2), 123);
        chunk.write_op(OpNegate, 123);
        chunk.write_op(OpReturn, 123);

        let mut vm = VM::new(chunk);
        vm.interpret();
        assert_eq!(stderr_string(), r#"stack []
0000  123 OP_CONSTANT     : 0000 '1.2'
stack [1.2,]
0002    | OP_NEGATE       :
stack [-1.2,]
0003    | OP_RETURN       :
"#);
    }

    fn eval(source: &str) {
        let result = std::panic::catch_unwind(|| {
            let scanner = Scanner::new(&source);
            let compiler = Compiler::new(scanner);
            let chunk = compiler.compile().unwrap();
            let mut vm: VM = VM::new(chunk);
            vm.interpret();
        });
        if result.is_err() {
            println!("{}", stderr_string())
        }
        assert!(result.is_ok())
    }

    #[test]
    fn test_compile() -> Result<(), LoxError> {
        let source = r#"print (-1 + 2) * 3 - -4;"#;
        let scanner = Scanner::new(&source);
        let compiler = Compiler::new(scanner);
        let chunk = compiler.compile()?;
        let mut vm: VM = VM::new(chunk);
        vm.interpret();
        assert_eq!(stdout_string(), "7\n");
        Ok(())
    }

    #[test]
    fn test_expression() -> Result<(), LoxError> {

        eval(r#"print !(5 - 4 > 3 * 2 == !nil);"#);
        assert_eq!("true\n", stdout_string());
        Ok(())
    }

    #[test]
    fn test_str() -> Result<(), LoxError> {

        eval(r#"print "a" + "b"; "#);
        assert_eq!(stdout_string(), "ab\n");
        Ok(())
    }
    #[test]
    fn test_global_var() -> Result<(), LoxError> {
        eval(r#"
        var beverage = "cafe au lait";
        var breakfast = "beignets";
        breakfast = "beignets with " + beverage;
        print breakfast;
        "#);
        assert_eq!(stdout_string(), "beignets with cafe au lait\n");
        Ok(())
    }

    #[test]
    fn test_loop() -> Result<(), LoxError> {
        eval(r#"
        var sum = 0;
        for (var i=0; i < 10 ; i = i + 1) {
            sum = sum + i;
        }
        print sum;
        "#);
        assert_eq!(stdout_string(), "45\n");
        Ok(())
    }
}