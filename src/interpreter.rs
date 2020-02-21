use crate::parser::{Visitor, Expr, StmtVisitor, Stmt};
use crate::scanner::TokenType::*;
use failure::Fail;

pub struct Interpreter;
use std::fmt;


#[derive(Debug, PartialEq)]
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
            Value::STRING(s) => {write!(f, "{}" ,s.as_str())},
            Value::NUMBER(v) => {write!(f, "{}", v)},
            Value::BOOL(b) => {write!(f, "{}", *b)},
            Value::OBJECT => {write!(f, "(object)")},
            Value::FUN => {write!(f, "(fn)")},
            Value::NIL => {write!(f, "nil")},
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
    DivideByZero
}

use RuntimeError::*;

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::BOOL(b) => *b,
            Value::STRING(s) => !s.is_empty(),
            Value::NUMBER(v) => *v != 0f64 ,
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

type RuntimeValue = Result<Value, RuntimeError>;

impl Interpreter {
    fn evaluate(&mut self, expr: &Expr) -> RuntimeValue {
        return expr.accept(self);
    }

    pub fn interpret(&mut self, expr: &Expr) -> Result<String, RuntimeError> {
        let value = self.evaluate(expr)?;
        Ok(format!("{}", value))
    }
}

impl StmtVisitor for Interpreter {
    fn visit_expr_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        let Stmt::ExprStmt(expr) = stmt;
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: &Stmt) {
        unimplemented!()
    }
}

impl Visitor<RuntimeValue> for Interpreter {
    fn visit_binary(&mut self, expr: &Expr) -> RuntimeValue {
        if let Expr::Binary(lhs, op, rhs) = expr {
            let left = self.evaluate(lhs)?;
            let right = self.evaluate(rhs)?;
            let result: Value = match op {
                MINUS => (left.check_number()? - right.check_number()?).into(),
                SLASH => {
                    let right = right.check_number()?;
                    if right == 0f64 {
                        return Err(DivideByZero)
                    }
                    (left.check_number()? / right).into()
                },
                STAR => (left.check_number()? * right.check_number()?).into(),
                PLUS => {
                    if let Value::STRING(s) = left  {
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
                _ => { return Err(InvalidBinaryOP)}
            };
            return Ok(result)
        }
        panic!("not a binary expr")
    }

    fn visit_grouping(&mut self, expr: &Expr) -> RuntimeValue {
        if let Expr::Grouping(expr) = expr {
            return self.evaluate(expr);
        }
        panic!("not a grouping expr")
    }


    fn visit_unary(&mut self, expr: &Expr) -> RuntimeValue {
        match expr {
            Expr::Unary(MINUS, rhs) => {
                let v = self.evaluate(rhs)?.check_number()?;
                Ok(Value::NUMBER(-v))
            }
            Expr::Unary(BANG, rhs) => {
                let v = self.evaluate(rhs)?;
                Ok(Value::BOOL(v.is_truthy()))
            }
            _ => {
                unimplemented!()
            }
        }
    }



    fn visit_literal(&mut self, expr: &Expr) -> RuntimeValue {
        match expr {
            Expr::Literal(STRING(s)) => Ok(Value::STRING(s.clone())),
            Expr::Literal(NUMBER(f)) => Ok(Value::NUMBER(*f)),
            Expr::Literal(NIL) => Ok(Value::NIL),
            Expr::Literal(TRUE) => Ok(Value::BOOL(true)),
            Expr::Literal(FALSE) => Ok(Value::BOOL(false)),
            _ => {
                Err(UnexpectedExpr { message: format!("{:?}", expr) })
            }
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use failure::Error;
    use crate::scanner::Scanner;
    use crate::parser::Parser;

    fn evalute(source: &str) -> Result<String, Error> {
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let parser = Parser::new(tokens);
        let expr = parser.parse()?;
        let mut interpreter = Interpreter {};
        let result = interpreter.interpret(&expr)?;
        Ok(result)
    }

    #[test]
    fn test_expr() -> Result<(), Error> {
        let source = "-1 + 2 * 3/(6-5)";
        let result = evalute(source)?;
        assert_eq!(result, String::from("5"));
        Ok(())
    }



    #[test]
    fn test_compare() -> Result<(), Error> {
        let source = "1 > 2";
        let result = evalute(source)?;
        assert_eq!(result, String::from("false"));
        Ok(())
    }

    #[test]
    fn test_string_plus() -> Result<(), Error> {
        let source = "1 + \"2\"";
        let result = evalute(source)?;
        assert_eq!(result, String::from("12"));
        Ok(())
    }
}