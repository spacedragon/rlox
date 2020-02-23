use crate::parser::{ Expr, Stmt, VisitorMut, StmtVisitorMut};
use std::collections::{LinkedList, HashMap};
use crate::scanner::Token;
use crate::scanner::TokenType::*;



use crate::error::ParserError;
use ParserError::*;


pub struct Resolver {
    scopes: LinkedList<HashMap<String, bool>>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: LinkedList::new(),
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push_front(HashMap::new())
    }

    fn end_scope(&mut self) {
        self.scopes.pop_front();
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt) {
        stmt.accept_mut(self).expect("unexpected failure.");
    }

    pub fn resolve(&mut self, stmts: &mut Vec<Stmt>) {
        for stmt in stmts {
            self.resolve_stmt(stmt)
        }
    }

    fn declare(&mut self, name: String) {
        if let Some(scope) = self.scopes.front_mut() {
            scope.insert(name, false);
        }
    }

    fn define(&mut self, name: String) {
        if let Some(scope) = self.scopes.front_mut() {
            scope.insert(name, true);
        }
    }

    fn resolve_expr(&mut self, expr: &mut Expr) {
        expr.accept_mut(self).expect("unexpected failure.");
    }

    fn resolve_local(&mut self, name: &str) -> i32 {
        let mut i = 0;
        for scope in self.scopes.iter() {
            if scope.contains_key(name) {
                return i;
            }
            i += 1;
        }
        return -1
    }

}



impl VisitorMut<Result<(), ParserError>> for Resolver {
    fn visit_binary(&mut self, expr: &mut Expr) -> Result<(), ParserError> {
        if let Expr::Binary(lhs, _, rhs) = expr {
            self.resolve_expr(lhs);
            self.resolve_expr(rhs);
        }
        Ok(())
    }

    fn visit_grouping(&mut self, expr: &mut Expr) -> Result<(), ParserError> {
        if let Expr::Grouping(e) = expr {
            self.resolve_expr(e);
        }
        Ok(())
    }

    fn visit_unary(&mut self, expr: &mut Expr) -> Result<(), ParserError> {
        if let Expr::Unary(_, rhs) = expr {
            self.resolve_expr(rhs);
        }
        Ok(())
    }

    fn visit_literal(&mut self, _expr: &mut Expr) -> Result<(), ParserError> {
        Ok(())
    }

    fn visit_var_expr(&mut self, expr: &mut Expr) -> Result<(), ParserError> {
        if let Expr::Variable(Token { token_type: IDENTIFIER(name), ..}, depth) = expr {
            if let Some(scope) = self.scopes.front() {
                if let Some(false) = scope.get(name) {
                    return Err(ReadLocalInInit);
                }
            }
            *depth = self.resolve_local(name);
        }
        return Ok(());
    }

    fn visit_assign(&mut self, expr: &mut Expr) -> Result<(), ParserError> {
        if let Expr::Assign(Token { token_type: IDENTIFIER(name), ..}, value,depth) = expr {
            self.resolve_expr(value);
            *depth = self.resolve_local(name);
        }
        return Ok(());
    }

    fn visit_logical(&mut self, expr: &mut Expr) -> Result<(), ParserError> {
        if let Expr::Logical(l,_ , r) = expr {
            self.resolve_expr(l);
            self.resolve_expr(r);
        }
        Ok(())
    }

    fn visit_call(&mut self, expr: &mut Expr) -> Result<(), ParserError> {
        if let Expr::Call(callee, _, args) = expr {
            self.resolve_expr(callee);
            for arg in args.iter_mut() {
                self.resolve_expr(arg);
            }
        }
        Ok(())
    }
}

impl StmtVisitorMut for Resolver {
    type Err =  ParserError;

    fn visit_expr_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        if let Stmt::ExprStmt(expr) = stmt {
            self.resolve_expr(expr)
        }
        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        if let Stmt::PrintStmt(expr) = stmt {
            self.resolve_expr(expr);
            return Ok(())
        }
        unreachable!()
    }

    fn visit_var_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        if let Stmt::VarStmt(Token{ token_type: IDENTIFIER(name), ..}, init) = stmt {
            self.declare(name.to_string());
            self.resolve_expr(init);
            self.define(name.to_string());
            return Ok(())
        }
        unreachable!()
    }

    fn visit_block_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        if let Stmt::Block(stmts) = stmt {
            self.begin_scope();
            self.resolve(stmts);
            self.end_scope();
            return Ok(());
        }
        unreachable!()
    }

    fn visit_if_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        if let Stmt::IfStmt(condition, then_stmt, else_stmt) = stmt {
            self.resolve_expr(condition);
            self.resolve_stmt(then_stmt);
            if let Some(el) = else_stmt {
                self.resolve_stmt(el)
            }
            return Ok(());
        }
        unreachable!()
    }

    fn visit_while_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        if let Stmt::WhileStmt(condition, body) = stmt {
            self.resolve_expr(condition);
            self.resolve_stmt(body)
        }
        Ok(())
    }

    fn visit_func_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        if let Stmt::Function(Token{ token_type: IDENTIFIER(name), ..}, params, body) = stmt {
            self.declare(name.to_string());
            self.define(name.to_string());
            self.begin_scope();
            for p in params {
                if let Token{ token_type: IDENTIFIER(param), ..} = p {
                    self.declare(param.to_string());
                    self.define(param.to_string());
                }
            }
            self.resolve(body);
            self.end_scope();
            return Ok(())
        }
        unreachable!()
    }

    fn visit_ret_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        if let Stmt::ReturnStmt(_ret, value) =  stmt {
            self.resolve_expr(value)
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::scanner::Scanner;
    use crate::parser::Parser;
    use crate::resolver::Resolver;
    use crate::error::LoxError;
    use crate::ast_printer::AstPrinter;

    #[test]
    fn test_resolver() -> Result<(), LoxError> {
        let source = "var a = 1;\n\
                            fun f1() {\n\
                                var b = 1; \n\
                                fun f2() { \n\
                                    var c = 2; \n\
                                    print b;\n\
                                    fun f3() {\n\
                                        print b;\n\
                                    }\n\
                                }\n\
                            }\
                            ";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let parser = Parser::new(tokens);
        let mut stmts = parser.parse()?;
        let mut resolver = Resolver::new();
        resolver.resolve(&mut stmts);
        let mut printer = AstPrinter::new();
        let result = printer.print_stmts(&stmts);
        assert!(result.contains("var b 1"));
        assert!(result.contains("var b 2"));
        Ok(())
    }
}
