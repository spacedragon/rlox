use crate::parser::{Visitor, Expr};
use crate::scanner::TokenType::*;
use crate::scanner::Token;

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
            Expr::Literal(Token{ token_type: NUMBER(ref v), .. }) => { format!("{}", v) }
            Expr::Literal(Token{ token_type:STRING(ref s), .. }) => { format!("\"{}\"", s) }
            Expr::Literal(Token{ token_type:IDENTIFIER(ref s), .. }) => { s.to_string() }
            Expr::Literal(ref op) => { format!("{}", op) }
            _=> panic!("not a literal expr.")
        }
    }

    fn visit_var(&mut self, _expr: &Expr) -> String {
        unimplemented!()
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
    use crate::scanner::{TokenType, Pos, Token};
    #[test]
    fn test_string() {
        let expression = Expr::Literal(Token::new(STRING(String::from("test")), Pos::default()));
        let mut printer = AstPrinter {};
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
        let mut printer = AstPrinter {};
        assert_eq!(expression.accept(& mut printer), String::from("(* (- 123) (group 45.67))"))
    }
}