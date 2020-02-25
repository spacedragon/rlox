use crate::parser::{Expr, Stmt, VisitorMut, StmtVisitorMut};
use std::collections::{LinkedList, HashMap};
use crate::scanner::Token;
use crate::scanner::TokenType::*;


use crate::error::ResolverError;
use crate::error::ResolverError::*;
use std::mem;


enum FunctionType {
    None,
    Function,
    Method,
    Initializer,
}

enum ClassType {
    None,
    Class,
    SUBCLASS
}

pub struct Resolver {
    scopes: LinkedList<HashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: LinkedList::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
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

    fn declare(&mut self, name: &str) -> Result<(), ResolverError> {
        if let Some(scope) = self.scopes.front_mut() {
            if scope.contains_key(name) {
                return Err(VariableAlreadyDeclared);
            } else {
                scope.insert(name.to_string(), false);
            }
        }
        Ok(())
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.front_mut() {
            scope.insert(name.to_string(), true);
        }
    }

    fn resolve_expr(&mut self, expr: &mut Expr) {
        expr.accept_mut(self).expect("unexpected failure.");
    }

    fn resolve_local(&mut self, name: &str) -> i32 {
        for (i,scope) in self.scopes.iter().enumerate() {
            if scope.contains_key(name) {
                return i as i32;
            }
        }
        -1
    }

    fn resolve_function(&mut self, stmt: &mut Stmt, fun_type: FunctionType) -> Result<(), ResolverError> {
        if let Stmt::Function(_, params, body) = stmt {
            let prev = std::mem::replace(&mut self.current_function, fun_type);

            self.begin_scope();
            for p in params {
                if let Token { token_type: IDENTIFIER(param), .. } = p {
                    self.declare(param)?;
                    self.define(param);
                }
            }
            self.resolve(body);
            self.end_scope();
            std::mem::replace(&mut self.current_function, prev);
        }
        Ok(())
    }
}


impl VisitorMut<Result<(), ResolverError>> for Resolver {
    fn visit_binary(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        if let Expr::Binary(lhs, _, rhs) = expr {
            self.resolve_expr(lhs);
            self.resolve_expr(rhs);
        }
        Ok(())
    }

    fn visit_grouping(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        if let Expr::Grouping(e) = expr {
            self.resolve_expr(e);
        }
        Ok(())
    }

    fn visit_unary(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        if let Expr::Unary(_, rhs) = expr {
            self.resolve_expr(rhs);
        }
        Ok(())
    }

    fn visit_literal(&mut self, _expr: &mut Expr) -> Result<(), ResolverError> {
        Ok(())
    }

    fn visit_var_expr(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        if let Expr::Variable(Token { token_type: IDENTIFIER(name), .. }, depth) = expr {
            if let Some(scope) = self.scopes.front() {
                if let Some(false) = scope.get(name) {
                    return Err(ReadLocalInInit);
                }
            }
            *depth = self.resolve_local(name);
        }
        Ok(())
    }

    fn visit_assign(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        if let Expr::Assign(Token { token_type: IDENTIFIER(name), .. }, value, depth) = expr {
            self.resolve_expr(value);
            *depth = self.resolve_local(name);
        }
        Ok(())
    }

    fn visit_logical(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        if let Expr::Logical(l, _, r) = expr {
            self.resolve_expr(l);
            self.resolve_expr(r);
        }
        Ok(())
    }

    fn visit_call(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        if let Expr::Call(callee, _, args) = expr {
            self.resolve_expr(callee);
            for arg in args.iter_mut() {
                self.resolve_expr(arg);
            }
        }
        Ok(())
    }

    fn visit_get(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        if let Expr::Get(expr, _) = expr {
            self.resolve_expr(expr);
        }
        Ok(())
    }

    fn visit_set(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        if let Expr::Set(expr, _, value) = expr {
            self.resolve_expr(expr);
            self.resolve_expr(value);
        }
        Ok(())
    }

    fn visit_this(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        if let ClassType::None = self.current_class {
            return Err(UseThisOutOfClass);
        }
        if let Expr::This(_, depth) = expr {
            *depth = self.resolve_local("this");
        }
        Ok(())
    }

    fn visit_super(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        match self.current_class {
            ClassType::None => { return Err(UseSuperOutOfClass); },
            ClassType::Class => { return Err(UseSuperOutOfSubClass); },
            ClassType::SUBCLASS => {},
        }
        if let Expr::Super(_,_, depth) = expr {
            *depth = self.resolve_local("super");
        }
        Ok(())
    }
}

impl StmtVisitorMut for Resolver {
    type Err = ResolverError;

    fn visit_expr_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        if let Stmt::ExprStmt(expr) = stmt {
            self.resolve_expr(expr)
        }
        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        if let Stmt::PrintStmt(expr) = stmt {
            self.resolve_expr(expr);
            return Ok(());
        }
        unreachable!()
    }

    fn visit_var_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        if let Stmt::VarStmt(Token { token_type: IDENTIFIER(name), .. }, init) = stmt {
            self.declare(name)?;
            self.resolve_expr(init);
            self.define(name);
            return Ok(());
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
        if let Stmt::Function(Token { token_type: IDENTIFIER(name), .. }, _, _) = stmt {
            self.declare(name)?;
            self.define(name);
            self.resolve_function(stmt, FunctionType::Function)?;
            return Ok(());
        }
        unreachable!()
    }

    fn visit_ret_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        match self.current_function {
            FunctionType::None => {
                Err(ReturnTopLevel)
            }
            FunctionType::Initializer=> {
                if let Stmt::ReturnStmt(_ret, Expr::Literal(Token{ token_type: NIL, .. })) = stmt {
                    Ok(())
                } else {
                    Err(ReturnInInit)
                }
            }
            _ => {
                if let Stmt::ReturnStmt(_ret, value) = stmt {
                    self.resolve_expr(value)
                }
                Ok(())
            }
        }
    }

    fn visit_class_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err> {
        if let Stmt::Class(Token { token_type: IDENTIFIER(name), .. }, methods, superclass) = stmt {
            let class_type = mem::replace(&mut self.current_class, ClassType::Class);
            self.declare(name)?;
            self.define(name);
            if let Some(sc) = superclass {
                if let Expr::Variable(Token { token_type: IDENTIFIER(supername), ..}, ..) = sc {
                    if supername == name {
                        return Err(InheritSelf);
                    }
                }
                mem::replace(&mut self.current_class, ClassType::SUBCLASS);
                self.resolve_expr(sc);
                self.begin_scope();
                self.scopes.front_mut().unwrap().insert("super".to_string(), true);
            }
            self.begin_scope();
            self.scopes.front_mut().unwrap().insert("this".to_string(), true);

            for method in methods {
                let mut d = FunctionType::Method;
                if let Stmt::Function(Token { token_type: IDENTIFIER(name), .. }, ..) = method {
                    if name == "init" {
                        d = FunctionType::Initializer;
                    }
                }
                self.resolve_function(method, d)?;
            }
            self.end_scope();
            if superclass.is_some() {
                self.end_scope();
            }
            mem::replace(&mut self.current_class, class_type);
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
