use crate::parser::{Visitor, Expr, StmtVisitor, Stmt};
use crate::scanner::TokenType::*;
use failure::Fail;

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
    #[fail(display = "Target is not callable.")]
    NotCallable,
    #[fail(display = "Expected {} arguments but got {}.", expect, actual)]
    ArgumentsSizeNotMatch { expect: i8, actual: i8 },
    #[fail(display = "early return")]
    ReturnValue(Value),
}

use RuntimeError::*;
use crate::scanner::Token;
use crate::string_writer::StringWriter;
use crate::environment::Environment;
use crate::value::{Value, Fun};
use std::rc::Rc;
use std::cell::RefCell;


type ValueResult = Result<Value, RuntimeError>;

pub struct Interpreter<W: StringWriter> {
    output: W,
    globals: Rc<RefCell<Environment>>,
    env: Rc<RefCell<Environment>>,
}

impl<W: StringWriter> Interpreter<W> {
    pub fn new(output: W) -> Self {
        let globals = Rc::new(RefCell::new(Environment::global()));
        Interpreter {
            output,
            env: globals.clone(),
            globals,
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


    fn execute_stmts(&mut self, stmts: &Vec<Stmt>, env: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
        let prev = self.env.clone();
        self.env = env;
        for stmt in stmts {
            if let Err(e) = self.execute(stmt) {
                self.env = prev;
                return Err(e);
            }
        }
        self.env = prev;

        Ok(())
    }

    fn execute_function(&mut self, callee: Value, args: Vec<Value>) -> ValueResult {
        if let Value::FUN(f) = callee {
            let actual = args.len() as i8;
            return if actual != f.arity() {
                Err(ArgumentsSizeNotMatch {
                    expect: f.arity(),
                    actual,
                })
            } else {
                return match f {
                    Fun::Native(_, _, f) => {
                        Ok(f(args))
                    }
                    Fun::UserFunc(_, _, Stmt::Function(_, params, body)) => {
                        let mut env = Environment::new(self.globals.clone());
                        for i in 0..params.len() {
                            if let Token { token_type: IDENTIFIER(name), .. } = &params[i] {
                                env.define(name.clone(), args[i].clone());
                            }
                        }
                        if let Err(ReturnValue(v)) = self.execute_stmts(&body,
                                                                        Rc::new(RefCell::new(env))) {
                            return Ok(v)
                        }

                        Ok(Value::NIL)
                    }
                    _ => { panic!("not a function") }
                };
            };
        }
        panic!("not a function")
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

            self.env.borrow_mut().define(name.clone(), value);
            return Ok(());
        }
        panic!("should not reach here!")
    }

    fn visit_block_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::Block(stmts) = stmt {
            let new_env = Environment::new(self.env.clone());
            self.execute_stmts(stmts, Rc::new(RefCell::new(new_env)))?;
            return Ok(());
        }
        panic!("should not reach here!")
    }

    fn visit_if_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::IfStmt(condition, then_branch, else_branch) = stmt {
            if self.evaluate(condition)?.is_truthy() {
                self.execute(then_branch)?;
            } else if let Some(else_branch) = else_branch {
                self.execute(else_branch)?;
            }
            return Ok(());
        }
        panic!("should not reach here!")
    }

    fn visit_while_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::WhileStmt(condition, body) = stmt {
            while self.evaluate(condition)?.is_truthy() {
                self.execute(body)?;
            }
            return Ok(());
        }
        panic!("should not reach here!")
    }

    fn visit_func_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::Function(Token { token_type: IDENTIFIER(name), .. },
                              params, _body) = stmt {
            let f: Fun = Fun::UserFunc(name.clone(), params.len() as i8, stmt.clone());
            self.env.borrow_mut().define(name.clone(), Value::FUN(f));
            return Ok(());
        }

        panic!("should not reach here!")
    }

    fn visit_ret_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::ReturnStmt(_token, value) = stmt {
            let value = self.evaluate(value)?;
            return Err(ReturnValue(value));
        }
        panic!("should not reach here!")
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
            return self.env.borrow().get(t);
        }
        panic!("not a var expr")
    }

    fn visit_assign(&mut self, expr: &Expr) -> ValueResult {
        if let Expr::Assign(t, expr) = expr {
            let value = self.evaluate(expr)?;
            self.env.borrow_mut().assign(t, &value)?;
            return Ok(value);
        }
        panic!("not a var expr")
    }

    fn visit_logical(&mut self, expr: &Expr) -> ValueResult {
        if let Expr::Logical(lhs, token, rhs) = expr {
            let left = self.evaluate(lhs)?;
            return match token.token_type {
                OR if left.is_truthy() => Ok(left),
                AND if !left.is_truthy() => Ok(left),
                _ => { self.evaluate(rhs) }
            };
        }
        panic!("not a logical expr")
    }

    fn visit_call(&mut self, expr: &Expr) -> ValueResult {
        if let Expr::Call(callee, _token, arguments) = expr {
            let callee = self.evaluate(callee)?;
            let mut args = vec![];
            for arg in arguments.iter() {
                args.push(self.evaluate(arg)?)
            }
            return self.execute_function(callee, args);
        }
        panic!("not a call expr")
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

    #[test]
    fn test_if() -> Result<(), Error> {
        let source = "if (true) { \
                                print \"yes\"; \
                            }\
                            if (\"hi\" and nil) {\
                                print true and \"yes\"; \
                            } else { \
                                print nil or \"no\"; \
                            }";
        let result = eval(source)?;
        assert_eq!(result, String::from("yes\nno\n"));
        Ok(())
    }

    #[test]
    fn test_while() -> Result<(), Error> {
        let source = "var b = 1; \
                            var a = 0; \
                            while (a < 10000) { \
                              print a; \
                              var temp = a; \
                              a = b; \
                              b = temp + b; \
                            }";
        let result = eval(source)?;
        assert_eq!(result, String::from("0\n\
                                            1\n\
                                            1\n\
                                            2\n\
                                            3\n\
                                            5\n\
                                            8\n\
                                            13\n\
                                            21\n\
                                            34\n\
                                            55\n\
                                            89\n\
                                            144\n\
                                            233\n\
                                            377\n\
                                            610\n\
                                            987\n\
                                            1597\n\
                                            2584\n\
                                            4181\n\
                                            6765\n"));
        Ok(())
    }

    #[test]
    fn test_native() -> Result<(), Error> {
        let source = "print clock();";
        let result = eval(source)?;
        println!("{}", result);

        assert!(!result.is_empty());
        Ok(())
    }

    #[test]
    fn test_func() -> Result<(), Error> {
        let source = "fun fibonacci(n) { \n\
                              if (n <= 1) return n; \n\
                              return fibonacci(n - 2) + fibonacci(n - 1); \n\
                            } \n\
                             \n\
                            for (var i = 0; i < 20; i = i + 1) { \n\
                              print fibonacci(i); \n\
                            }";
        let result = eval(source)?;


        assert_eq!(result, "0\n\
                            1\n\
                            1\n\
                            2\n\
                            3\n\
                            5\n\
                            8\n\
                            13\n\
                            21\n\
                            34\n\
                            55\n\
                            89\n\
                            144\n\
                            233\n\
                            377\n\
                            610\n\
                            987\n\
                            1597\n\
                            2584\n\
                            4181\n");
        Ok(())
    }
}