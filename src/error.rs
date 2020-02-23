use quick_error::quick_error;
use crate::value::Value;

quick_error!{
    #[derive(Debug)]
    pub enum LoxError {
        ParseError(err: ParserError) {
            from()
        }
        RuntimeError(err: RuntimeError) {
            from()
        }
        ScannerError(err: ScannerError) {
            from()
        }
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum ScannerError {
        UnexpectedCharacter(ch: char,line: usize) {
            display("Unexpected character {} on line {}.", ch, line)
        }
        UnterminatedString(line: usize) {
            display("Unterminated string on line {}.", line)
        }
        InvalidNumber(line: usize,str: String) {
            display("Invalid Number string {} on line {}.", str, line)
        }
    }
}

quick_error!{
    #[derive(Debug)]
    pub enum ParserError {
        ExpectExpr(line: usize) {
            display("Expect expression.(line {})", line)
        }
        ExpectRightParen ( line: usize ) {
            display("Expect ')' after expression. (line {})", line)
        }
        ExpectSemi ( line: usize ) {
            display("Expect ';' after expression. (line {})", line)
        }
        ExpectVarName( line: usize) {
            display("Expect variable name.(line {})", line)
        }
        InvalidAssign ( line: usize ) {
            display("Invalid assignment target. (line {})", line)
        }
        ExpectRightBrace ( line: usize ) {
            display("Expect '}}' after block. (line {})", line)
        }
        ExpectIfParen ( line: usize ) {
            display("Expect '()' after 'if'. (line {})", line)
        }
        ExpectWhileParen ( line: usize ) {
            display ("Expect '()' after 'while'. (line {})", line)
        }
        ExpectForParen ( line: usize ) {
            display ("Expect '()' after 'for'. (line {})", line)
        }
        ExpectError { msg: String, line: usize } {
            display ("{}(line {})", msg, line)
        }
        ArgumentsExceedError ( line: usize ){
            display ("Cannot have more than 255 arguments.(line {})", line)
        }
        ReadLocalInInit {
            display ("Cannot read local variable in its own initializer.")
        }
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum RuntimeError {
        OperandMustNumber {
            display("Operand must be a number.")
        }
        UnexpectedExpr(message: String) {
            display("Unexpected expression {:?}.", message)
        }
        InvalidBinaryOP {
            display("Invalid binary operation.")
        }
        DivideByZero {
            display("Divide by zero.")
        }
        UndefinedVar(name: String) {
            display("Undefined variable {}.", name)
        }
        ExpectIdentifier {
            display("Expected a identifier here")
        }
        NotCallable {
            display("Target is not callable.")
        }
        ArgumentsSizeNotMatch(expect: i8, actual: i8) {
            display("Expected {} arguments but got {}.", expect, actual)
        }
        ReturnValue(value: Value) {}
    }
}



