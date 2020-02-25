use crate::parser::{Visitor, Expr, StmtVisitor, Stmt};
use crate::scanner::TokenType::*;
use crate::scanner::Token;

pub struct AstPrinter{
    s: String
}

impl Visitor<String> for AstPrinter {

    fn visit_binary(&mut self, expr: &Expr) -> String {
        if let Expr::Binary(ref lhs, ref op, ref rhs) = expr {
            return format!("({} {} {})", op, lhs.accept(self), rhs.accept(self))
        }
        unreachable!()
    }

    fn visit_grouping(&mut self, expr: &Expr) -> String {
        if let Expr::Grouping(expr) = expr {
            return format!("(group {})", expr.accept(self))
        }
        unreachable!()
    }

    fn visit_unary(&mut self, expr: &Expr) -> String {
        if let Expr::Unary(ref op, ref rhs) = expr {
            return format!("({} {})", op, rhs.accept(self))
        }
        unreachable!()
    }

    fn visit_literal(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Literal(Token{ token_type: NUMBER(ref v), .. }) => { format!("{}", v) }
            Expr::Literal(Token{ token_type:STRING(ref s), .. }) => { format!("\"{}\"", s) }
            Expr::Literal(Token{ token_type:IDENTIFIER(ref s), .. }) => { s.to_string() }
            Expr::Literal(ref op) => { format!("{}", op) }
            _=> panic!("not a literal expr.")
        }
    }

    fn visit_var_expr(&mut self, expr: &Expr) -> String {
        if let Expr::Variable(Token{ token_type: IDENTIFIER(name), .. }, depth) = expr {
            return format!("(var {} {})", name, depth)
        }
        unreachable!()
    }

    fn visit_assign(&mut self, expr: &Expr) -> String {
        if let Expr::Assign(ref t, _, depth) = expr {
            return format!("(assign {} d{})", t, depth)
        }
        unreachable!()
    }

    fn visit_logical(&mut self, expr: &Expr) -> String {
        if let Expr::Logical(_, ref t, _) = expr {
            return format!("(logical {})", t, )
        }
        unreachable!()
    }

    fn visit_call(&mut self, expr: &Expr) -> String {
        if let Expr::Call(callee, ..) = expr {
            return format!("(call {:?})", callee)
        }
        unreachable!()
    }

    fn visit_get(&mut self, expr: &Expr) -> String {
        if let Expr::Get(object, ..) = expr {
            return format!("(get {:?})", object)
        }
        unreachable!()
    }

    fn visit_set(&mut self, expr: &Expr) -> String {
        if let Expr::Set(object, _, value) = expr {
            return format!("(set {:?} {:?})", object, value)
        }
        unreachable!()
    }

    fn visit_this(&mut self, expr: &Expr) -> String {
        if let Expr::This(_,t) = expr {
            return format!("(this {:?})", t)
        }
        unreachable!()
    }

    fn visit_super(&mut self, expr: &Expr) -> String {
        if let Expr::Super(_,t, _) = expr {
            return format!("(super {:?})", t)
        }
        unreachable!()
    }
}

impl StmtVisitor for AstPrinter {
    type Err = ();

    fn visit_expr_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::ExprStmt(expr) = stmt {
            let s = expr.accept(self);
            self.s.push_str(format!("<{}>\n", s).as_str());
        }
        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::PrintStmt(expr) = stmt {
            let s = expr.accept(self);
            self.s.push_str(format!("<print {}>\n", s).as_str());
        }
        Ok(())
    }

    fn visit_var_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::VarStmt(Token{ token_type: IDENTIFIER(name), .. }, expr) = stmt {
            let s = expr.accept(self);
            self.s.push_str(format!("<var {}={}>\n", name, s).as_str());
        }
        Ok(())
    }

    fn visit_block_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::Block(stmts) = stmt {
            let t = String::new();
            for stmt in stmts {
                stmt.accept(self)?;
            }
            self.s.push_str(format!("<block {}>\n", t).as_str());
        }
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::IfStmt(condition, then, else_branch) = stmt {
            let c = condition.accept(self);
            then.accept(self)?;
            if let Some(s) = else_branch {
                s.accept(self)?;
            }
            self.s.push_str(format!("<if {}>\n", c).as_str());
        }
        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::WhileStmt(cod, body) = stmt {
            let c = cod.accept(self);
             body.accept(self)?;
            self.s.push_str(format!("<while {}>\n", c).as_str());
        }
        Ok(())
    }

    fn visit_func_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::Function(name, _params, stmts) = stmt {
            let  t = String::new();
            for stmt in stmts {
                stmt.accept(self)?;
            }
            self.s.push_str(format!("<fun {} {}>\n", name, t ).as_str());
        }
        Ok(())
    }

    fn visit_ret_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::ReturnStmt(_t, expr) = stmt {
            let e = expr.accept(self);
            self.s.push_str(format!("<ret {}>\n", e).as_str());
        }
        Ok(())
    }

    fn visit_class_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> {
        if let Stmt::Class(Token { token_type: IDENTIFIER(name), ..}, stmts, _) = stmt {
            for stmt in stmts {
                stmt.accept(self)?;
            }
            self.s.push_str(format!("<class {}>\n", name).as_str());
        }
        Ok(())
    }
}

impl AstPrinter {
    pub fn new() -> Self{
        AstPrinter {
            s: String::new()
        }
    }
    pub fn print(&mut self, expr: &Expr) -> String {
        expr.accept(self)
    }

    pub fn print_stmts(&mut self, stmts: &[Stmt]) -> &str {
        for stmt in stmts {
            stmt.accept(self).expect("error!");
        }
        self.s.as_str()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanner::{TokenType, Pos, Token};
    #[test]
    fn test_string() {
        let expression = Expr::Literal(Token::new(STRING(String::from("test")), Pos::default()));
        let mut printer = AstPrinter::new();
        assert_eq!(expression.accept(&mut printer), String::from("\"test\""))
    }

    #[test]
    fn test_printer() {
        let expression = Expr::Binary(
            Box::new(Expr::Unary(
                Token::new(TokenType::MINUS, Pos::default()),
                Box::new(Expr::Literal(Token::new(TokenType::NUMBER(123f64), Pos::default())))
            )),
            Token::new(TokenType::STAR, Pos::default()),
            Box::new(Expr::Grouping(
                Box::new(Expr::Literal(
                    Token::new(TokenType::NUMBER(45.67), Pos::default())
                ))
            ))

        );
        let mut printer = AstPrinter::new();
        assert_eq!(expression.accept(& mut printer), String::from("(* (- 123) (group 45.67))"))
    }
}