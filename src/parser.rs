use crate::scanner::TokenType::*;
use crate::scanner::{Token, TokenType};
use failure::Fail;

pub trait Visitor<R> {
    fn visit_binary(&mut self, expr: &Expr) -> R;
    fn visit_grouping(&mut self, expr: &Expr) -> R;
    fn visit_unary(&mut self, expr: &Expr) -> R;
    fn visit_literal(&mut self, expr: &Expr) -> R;
    fn visit_var(&mut self, expr: &Expr) -> R;
    fn visit_assign(&mut self, expr: &Expr) -> R;
}

pub trait StmtVisitor {
    type Err;
    fn visit_expr_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err> ;
    fn visit_print_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err>;
    fn visit_var_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err>;
    fn visit_block_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err>;
}

#[derive(Debug)]
pub enum Expr {
    Assign(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Literal(Token),
    Grouping(Box<Expr>),
    Unary(Token, Box<Expr>),
    Variable(Token),
}

impl Expr {
    pub fn accept<R>(&self, visitor: &mut dyn Visitor<R>) -> R {
        match self {
            Expr::Binary(_, _, _) => visitor.visit_binary(self),
            Expr::Literal(_) => visitor.visit_literal(self),
            Expr::Grouping(_) => visitor.visit_grouping(self),
            Expr::Unary(_, _) => visitor.visit_unary(self),
            Expr::Variable(_) => visitor.visit_var(self),
            Expr::Assign(_, _) => visitor.visit_assign(self)
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(Expr),
    PrintStmt(Expr),
    VarStmt(Token, Expr),
    Block(Vec<Stmt>),
}

impl Stmt {
    pub fn accept<E>(&self, visitor: &mut dyn StmtVisitor<Err = E>) -> Result<(), E> {
        match self {
            Stmt::ExprStmt(_) => { visitor.visit_expr_stmt(self) }
            Stmt::PrintStmt(_) => { visitor.visit_print_stmt(self) }
            Stmt::VarStmt(_, _) => { visitor.visit_var_stmt(self) }
            Stmt::Block(_) => { visitor.visit_block_stmt(self)}
        }
    }
}

#[derive(Debug, Fail)]
pub enum ParserError {
    #[fail(display = "Expect expression.(line {})", line)]
    ExpectExpr { line: usize },
    #[fail(display = "Expect ')' after expression. (line {})", line)]
    ExpectRightParen { line: usize },
    #[fail(display = "Expect ';' after expression. (line {})", line)]
    ExpectSemi { line: usize },
    #[fail(display = "Expect variable name.")]
    ExpectVarName,
    #[fail(display = "Invalid assignment target. (line {})", line)]
    InvalidAssign { line: usize },
    #[fail(display = "Expect '}}' after block. (line {})",  line)]
    ExpectRightBrace { line: usize },
}

use ParserError::*;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut result = Vec::new();
        while !self.is_at_end() {
            result.push(self.declaration()?);
        }
        Ok(result)
    }

    fn declaration(&mut self) -> Result<Stmt, ParserError> {
        if self.matches(vec![VAR]) {
            return self.var_declaration();
        }
        self.statement()
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        let token = self.peek();
        if let IDENTIFIER(ref _name) = token.token_type {
            let name = token.clone();
            let line = token.pos.line;
            self.advance();
            let initializer: Expr = if self.check(&EQUAL) {
                self.advance();
                self.expression()?
            } else {
                Expr::Literal(Token::new(NIL, name.pos.clone()))
            };
            self.consume(&SEMICOLON, || ExpectSemi { line })?;
            return Ok(Stmt::VarStmt(name, initializer));
        }
        Err(ExpectVarName)
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.matches(vec![PRINT]) {
            return self.print_stmt();
        }
        if self.matches(vec![LEFT_BRACE]) {
            return self.block()
        }
        self.expr_stmt()
    }

    fn block(&mut self) -> Result<Stmt, ParserError> {
        let mut stmts = vec![];
        while !self.check(&RIGHT_BRACE) && !self.is_at_end() {
            let stmt = self.declaration()?;
            stmts.push(stmt);
        }
        let line = self.previous().pos.line;
        self.consume(&RIGHT_BRACE, || ExpectRightBrace { line })?;
        Ok(Stmt::Block(stmts))
    }

    fn print_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        let line = self.previous().pos.line;
        self.consume(&SEMICOLON, || ExpectSemi { line })?;
        Ok(Stmt::PrintStmt(expr))
    }

    fn expr_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        let line = self.previous().pos.line;
        self.consume(&SEMICOLON, || ExpectSemi { line })?;
        Ok(Stmt::ExprStmt(expr))
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || self.peek().token_type == EOF
    }

    pub fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParserError> {
        let expr = self.equality()?;
        if self.matches(vec![EQUAL]) {
            let equals = self.previous();
            let line = equals.pos.line;
            let value = self.assignment()?;
            match expr {
                Expr::Variable(t) => {
                    return Ok(Expr::Assign(t, Box::new(value)));
                }
                _ => {
                    return Err(ParserError::InvalidAssign { line })
                }
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while self.matches(vec![BANG_EQUAL, EQUAL_EQUAL]) {
            let right = self.comparison()?;
            let op = self.previous().clone();
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        Result::Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.addition()?;
        while self.matches(vec![GREATER_EQUAL, GREATER, LESS, LESS_EQUAL]) {
            let op = self.previous().clone();
            let right = self.addition()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right))
        }
        Result::Ok(expr)
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn check(&self, token_type: &TokenType) -> bool {
        !self.is_at_end() && self.peek().token_type == *token_type
    }

    fn matches(&mut self, token_types: Vec<TokenType>) -> bool {
        token_types.iter().any(|t| {
            if self.check(t) {
                self.advance();
                true
            } else {
                false
            }
        })
    }

    fn addition(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.multiplication()?;
        while self.matches(vec![MINUS, PLUS]) {
            let op = self.previous().clone();
            let right = self.multiplication()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right))
        }
        Result::Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while self.matches(vec![SLASH, STAR]) {
            let op = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right))
        }
        Result::Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if self.matches(vec![BANG, MINUS]) {
            let op = self.previous().clone();
            let right = self.unary()?;
            return Result::Ok(Expr::Unary(op, Box::new(right)));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        let t = self.peek();
        let line = t.pos.line;

        match t.token_type {
            NUMBER(_) | STRING(_) | TRUE | FALSE | NIL => {
                let tt = t.clone();
                self.advance();
                Ok(Expr::Literal(tt))
            }
            LEFT_PAREN => {
                self.advance();
                let expr = self.expression()?;
                self.consume(&RIGHT_PAREN,
                             || ParserError::ExpectRightParen { line })?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            IDENTIFIER(_) => {
                let token = t.clone();
                self.advance();
                Ok(Expr::Variable(token))
            }
            _ => Err(ParserError::ExpectExpr { line }),
        }
    }
    fn consume<F>(&mut self, token_type: &TokenType, on_err: F) -> Result<&Token, ParserError>
        where
            F: FnOnce() -> ParserError,
    {
        if self.check(token_type) {
            self.advance();
            Ok(self.previous())
        } else {
            Err(on_err())
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast_printer::AstPrinter;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use failure::Error;

    #[test]
    fn test_expr() -> Result<(), Error> {
        let source = "-1 + 2 * 3/(6-5)";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.expression()?;
        let mut printer = AstPrinter {};
        assert_eq!(
            printer.print(&expr),
            "(+ (- 1) (/ (* 2 3) (group (- 6 5))))"
        );
        Ok(())
    }
}
