use crate::parser::{Visitor, Expr};
use crate::scanner::TokenType::*;

pub struct AstPrinter;

impl Visitor<String> for AstPrinter {

    fn visit_binary(&mut self, expr: &Expr) -> String {
        if let Expr::Binary(ref lhs, ref op, ref rhs) = expr {
            return format!("({} {} {})", op, lhs.accept(self), rhs.accept(self))
        }
        panic!("not a binary expr")
    }

    fn visit_grouping(&mut self, expr: &Expr) -> String {
        if let Expr::Grouping(expr) = expr {
            return format!("(group {})", expr.accept(self))
        }
        panic!("not a grouping expr")
    }

    fn visit_unary(&mut self, expr: &Expr) -> String {
        if let Expr::Unary(ref op, ref rhs) = expr {
            return format!("({} {})", op, rhs.accept(self))
        }
        panic!("not a unary expr")
    }

    fn visit_literal(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Literal(NUMBER(ref v)) => { format!("{}", v) }
            Expr::Literal(STRING(ref s)) => { format!("\"{}\"", s) }
            Expr::Literal(IDENTIFIER(ref s)) => { format!("{}", s) }
            Expr::Literal(ref op) => { format!("{}", op) }
            _=> panic!("not a literal expr.")
        }
    }
}

impl AstPrinter {
    pub fn print(&mut self, expr: &Expr) -> String {
        expr.accept(self)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanner::TokenType;
    #[test]
    fn test_string() {
        let expression = Expr::Literal(STRING(String::from("test")));
        let mut printer = AstPrinter {};
        assert_eq!(expression.accept(&mut printer), String::from("\"test\""))
    }

    #[test]
    fn test_printer() {
        let expression = Expr::Binary(
            Box::new(Expr::Unary(
                TokenType::MINUS,
                Box::new(Expr::Literal(TokenType::NUMBER(123f64)))
            )),
            TokenType::STAR,
            Box::new(Expr::Grouping(
                Box::new(Expr::Literal(
                    TokenType::NUMBER(45.67)
                ))
            ))

        );
        let mut printer = AstPrinter {};
        assert_eq!(expression.accept(& mut printer), String::from("(* (- 123) (group 45.67))"))
    }
}