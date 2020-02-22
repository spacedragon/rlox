use crate::parser::{Visitor, Expr, StmtVisitor, Stmt};
use crate::scanner::TokenType::*;
use failure::Fail;

use std::fmt;


#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    STRING(String),
    NUMBER(f64),
    BOOL(bool),
    OBJECT,
    FUN,
    NIL,
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Value::NUMBER(v)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::STRING(s)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::STRING(String::from(s))
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::BOOL(b)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::STRING(s) => { write!(f, "{}", s.as_str()) }
            Value::NUMBER(v) => { write!(f, "{}", v) }
            Value::BOOL(b) => { write!(f, "{}", *b) }
            Value::OBJECT => { write!(f, "(object)") }
            Value::FUN => { write!(f, "(fn)") }
            Value::NIL => { write!(f, "nil") }
        }
    }
}

#[derive(Debug, Fail)]
pub enum RuntimeError {
    #[fail(display = "Operand must be a number.")]
    OperandMustNumber,
    #[fail(display = "Unexpected expression {:?}.", message)]
    UnexpectedExpr {
        message: String
    },
    #[fail(display = "Invalid binary operation.")]
    InvalidBinaryOP,
    #[fail(display = "Divide by zero.")]
    DivideByZero,
    #[fail(display = "Undefined variable {}.", name)]
    UndefinedVar {
        name: String
    },
    #[fail(display = "Expected a identifier here")]
    ExpectIdentifier,
}

use RuntimeError::*;
use crate::scanner::Token;
use crate::string_writer::StringWriter;
use crate::environment::Environment;

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::BOOL(b) => *b,
            Value::STRING(s) => !s.is_empty(),
            Value::NUMBER(v) => *v != 0f64,
            Value::NIL => false,
            _ => true
        }
    }

    fn check_number(&self) -> Result<f64, RuntimeError> {
        match self {
            Value::NUMBER(v) => Ok(*v),
            _ => Err(OperandMustNumber {})
        }
    }
}

type ValueResult = Result<Value, RuntimeError>;

pub struct Interpreter<W: StringWriter> {
    output: W,
    env: Box<Environment>,
}

impl<W: StringWriter> Interpreter<W> {
    pub fn new(output: W) -> Self {
        Interpreter {
            output,
            env: Box::new(Environment::new()),
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> ValueResult {
        expr.accept(self)
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<(), RuntimeError> {
        for statement in statements {
            self.execute(statement)?;
        }
        Ok(())
    }
    fn execute(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        stmt.accept(self)
    }
    fn execute_block(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        if let Stmt::Block(stmts) = stmt {
            self.env.new_scope();
            for stmt in stmts {
                self.execute(stmt)?;
            }
            self.env.pop_scope();
        }
        Ok(())
    }
}

impl<W: StringWriter> StmtVisitor for Interpreter<W> {
    type Err = RuntimeError;

    fn visit_expr_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::ExprStmt(expr) = stmt {
            self.evaluate(expr)?;
            return Ok(());
        }
        panic!("should not reach here!")
    }

    fn visit_print_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::PrintStmt(expr) = stmt {
            let value = self.evaluate(expr)?;
            self.output.write_string(format!("{}\n", value).as_str());
            return Ok(());
        }
        panic!("should not reach here!")
    }

    fn visit_var_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::VarStmt(Token { token_type: IDENTIFIER(name), .. }, init) = stmt {
            let value = self.evaluate(init)?;
            self.env.define(name.clone(), value);
            return Ok(());
        }
        panic!("should not reach here!")
    }

    fn visit_block_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
       self.execute_block(stmt)
    }
}

impl<W: StringWriter> Visitor<ValueResult> for Interpreter<W> {
    fn visit_binary(&mut self, expr: &Expr) -> ValueResult {
        if let Expr::Binary(lhs, op, rhs) = expr {
            let left = self.evaluate(lhs)?;
            let right = self.evaluate(rhs)?;
            let result: Value = match op.token_type {
                MINUS => (left.check_number()? - right.check_number()?).into(),
                SLASH => {
                    let right = right.check_number()?;
                    if right == 0f64 {
                        return Err(DivideByZero);
                    }
                    (left.check_number()? / right).into()
                }
                STAR => (left.check_number()? * right.check_number()?).into(),
                PLUS => {
                    if let Value::STRING(s) = left {
                        format!("{}{}", s, right).into()
                    } else if let Value::STRING(r) = right {
                        (format!("{}{}", left, r)).into()
                    } else {
                        (left.check_number()? + right.check_number()?).into()
                    }
                }
                GREATER => (left.check_number()? > right.check_number()?).into(),
                GREATER_EQUAL => (left.check_number()? >= right.check_number()?).into(),
                LESS => (left.check_number()? < right.check_number()?).into(),
                LESS_EQUAL => (left.check_number()? <= right.check_number()?).into(),
                BANG_EQUAL => (left != right).into(),
                EQUAL_EQUAL => (left == right).into(),
                _ => { return Err(InvalidBinaryOP); }
            };
            return Ok(result);
        }
        panic!("not a binary expr")
    }

    fn visit_grouping(&mut self, expr: &Expr) -> ValueResult {
        if let Expr::Grouping(expr) = expr {
            return self.evaluate(expr);
        }
        panic!("not a grouping expr")
    }


    fn visit_unary(&mut self, expr: &Expr) -> ValueResult {
        match expr {
            Expr::Unary(Token { token_type: MINUS, .. }, rhs) => {
                let v = self.evaluate(rhs)?.check_number()?;
                Ok(Value::NUMBER(-v))
            }
            Expr::Unary(Token { token_type: BANG, .. }, rhs) => {
                let v = self.evaluate(rhs)?;
                Ok(Value::BOOL(v.is_truthy()))
            }
            _ => {
                unimplemented!()
            }
        }
    }


    fn visit_literal(&mut self, expr: &Expr) -> ValueResult {
        match expr {
            Expr::Literal(token) => match token.token_type {
                STRING(ref s) => Ok(Value::STRING(s.clone())),
                NUMBER(ref f) => Ok(Value::NUMBER(*f)),
                NIL => Ok(Value::NIL),
                TRUE => Ok(Value::BOOL(true)),
                FALSE => Ok(Value::BOOL(false)),
                _ => {
                    Err(UnexpectedExpr { message: format!("{:?}", expr) })
                }
            },
            _ => {
                Err(UnexpectedExpr { message: format!("{:?}", expr) })
            }
        }
    }

    fn visit_var(&mut self, expr: &Expr) -> ValueResult {
        if let Expr::Variable(t) = expr {
            return self.env.get(t);
        }
        panic!("not a var expr")
    }

    fn visit_assign(&mut self, expr: &Expr) -> ValueResult {
        if let Expr::Assign(t, expr) = expr {
            let value = self.evaluate(expr)?;
            self.env.assign(t, &value)?;
            return Ok(value);
        }
        panic!("not a var expr")
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use failure::Error;
    use crate::scanner::Scanner;
    use crate::parser::Parser;

    fn eval(source: &str) -> Result<String, Error> {
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let parser = Parser::new(tokens);
        let stmts = parser.parse()?;
        let output = String::new();
        let mut interpreter = Interpreter::new(output);
        interpreter.interpret(&stmts)?;
        Ok(interpreter.output)
    }

    #[test]
    fn test_expr() -> Result<(), Error> {
        let source = "print -1 + 2 * 3/(6-5);";
        let result = eval(source)?;
        assert_eq!(result, String::from("5\n"));
        Ok(())
    }


    #[test]
    fn test_compare() -> Result<(), Error> {
        let source = "print 1 > 2;";
        let result = eval(source)?;
        assert_eq!(result, String::from("false\n"));
        Ok(())
    }

    #[test]
    fn test_string_plus() -> Result<(), Error> {
        let source = "print 1 + \"2\";";
        let result = eval(source)?;
        assert_eq!(result, String::from("12\n"));
        Ok(())
    }

    #[test]
    fn test_stmts() -> Result<(), Error> {
        let source = "print  \"one\";\n print true; print 2+1;";
        let result = eval(source)?;
        assert_eq!(result, String::from("one\ntrue\n3\n"));
        Ok(())
    }

    #[test]
    fn test_vars() -> Result<(), Error> {
        let source = "var a = 1;\n\
                            var b = 2;\n\
                            print a + b;\n";
        let result = eval(source)?;
        assert_eq!(result, String::from("3\n"));

        let source = "var a = 1;\n\
                            print a = 2;\n";
        let result = eval(source)?;
        assert_eq!(result, String::from("2\n"));
        Ok(())
    }

    #[test]
    fn test_scope() -> Result<(), Error> {
        let source = "var a = \"global a\"; \
                            var b = \"global b\"; \
                            var c = \"global c\"; \
                            {\
                              var a = \"outer a\"; \
                              var b = \"outer b\"; \
                              {\
                                var a = \"inner a\";\
                                print a;\
                                print b;\
                                print c;\
                              }\
                              print a;\
                              print b;\
                              print c;\
                            }\
                            print a;\
                            print b;\
                            print c;";
        let result = eval(source)?;
        assert_eq!(result, String::from("inner a\n\
                                            outer b\n\
                                            global c\n\
                                            outer a\n\
                                            outer b\n\
                                            global c\n\
                                            global a\n\
                                            global b\n\
                                            global c\n"));

        Ok(())
    }
}