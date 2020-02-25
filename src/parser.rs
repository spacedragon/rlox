use crate::scanner::TokenType::*;
use crate::scanner::{Token, TokenType};

pub trait Visitor<R> {
    fn visit_binary(&mut self, expr: &Expr) -> R;
    fn visit_grouping(&mut self, expr: &Expr) -> R;
    fn visit_unary(&mut self, expr: &Expr) -> R;
    fn visit_literal(&mut self, expr: &Expr) -> R;
    fn visit_var_expr(&mut self, expr: &Expr) -> R;
    fn visit_assign(&mut self, expr: &Expr) -> R;
    fn visit_logical(&mut self, expr: &Expr) -> R;
    fn visit_call(&mut self, expr: &Expr) -> R;
    fn visit_get(&mut self, expr: &Expr) -> R;
    fn visit_set(&mut self, expr: &Expr) -> R;
    fn visit_this(&mut self, expr: &Expr) -> R;
}

pub trait VisitorMut<R> {
    fn visit_binary(&mut self, expr: &mut Expr) -> R;
    fn visit_grouping(&mut self, expr: &mut Expr) -> R;
    fn visit_unary(&mut self, expr: &mut Expr) -> R;
    fn visit_literal(&mut self, expr: &mut Expr) -> R;
    fn visit_var_expr(&mut self, expr: &mut Expr) -> R;
    fn visit_assign(&mut self, expr: &mut Expr) -> R;
    fn visit_logical(&mut self, expr: &mut Expr) -> R;
    fn visit_call(&mut self, expr: &mut Expr) -> R;
    fn visit_get(&mut self, expr: &mut Expr) -> R;
    fn visit_set(&mut self, expr: &mut Expr) -> R;
    fn visit_this(&mut self, expr: &mut Expr) -> R;
}

pub trait StmtVisitor {
    type Err;
    fn visit_expr_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err>;
    fn visit_print_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err>;
    fn visit_var_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err>;
    fn visit_block_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err>;
    fn visit_if_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err>;
    fn visit_while_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err>;
    fn visit_func_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err>;
    fn visit_ret_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err>;
    fn visit_class_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Err>;
}

pub trait StmtVisitorMut {
    type Err;
    fn visit_expr_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err>;
    fn visit_print_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err>;
    fn visit_var_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err>;
    fn visit_block_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err>;
    fn visit_if_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err>;
    fn visit_while_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err>;
    fn visit_func_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err>;
    fn visit_ret_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err>;
    fn visit_class_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Err>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assign(Token, Box<Expr>, i32),
    Binary(Box<Expr>, Token, Box<Expr>),
    Call(Box<Expr>, Token, Box<Vec<Expr>>),
    Get(Box<Expr>, Token),
    Literal(Token),
    Logical(Box<Expr>, Token, Box<Expr>),
    Set(Box<Expr>, Token, Box<Expr>),
    This(Token, i32),
    Grouping(Box<Expr>),
    Unary(Token, Box<Expr>),
    Variable(Token, i32),
}




impl Expr {
    pub fn accept<R>(&self, visitor: &mut dyn Visitor<R>) -> R {
        match self {
            Expr::Binary(_, _, _) => visitor.visit_binary(self),
            Expr::Literal(_) => visitor.visit_literal(self),
            Expr::Grouping(_) => visitor.visit_grouping(self),
            Expr::Unary(_, _) => visitor.visit_unary(self),
            Expr::Variable(_,_) => visitor.visit_var_expr(self),
            Expr::Assign(_, _, _) => visitor.visit_assign(self),
            Expr::Logical(_, _, _) => visitor.visit_logical(self),
            Expr::Call(_, _, _) => visitor.visit_call(self),
            Expr::Get(_, _) => {visitor.visit_get(self)}
            Expr::Set(_, _, _) => {visitor.visit_set(self)}
            Expr::This(_, _) => {visitor.visit_this(self)}
        }
    }

    pub fn accept_mut<R>(&mut self, visitor: &mut dyn VisitorMut<R>) -> R {
        match self {
            Expr::Binary(_, _, _) => visitor.visit_binary(self),
            Expr::Literal(_) => visitor.visit_literal(self),
            Expr::Grouping(_) => visitor.visit_grouping(self),
            Expr::Unary(_, _) => visitor.visit_unary(self),
            Expr::Variable(_,_) => visitor.visit_var_expr(self),
            Expr::Assign(_, _, _) => visitor.visit_assign(self),
            Expr::Logical(_, _, _) => visitor.visit_logical(self),
            Expr::Call(_, _, _) => visitor.visit_call(self),
            Expr::Get(_, _) => {visitor.visit_get(self)},
            Expr::Set(_, _, _) => {visitor.visit_set(self)},
            Expr::This(_, _) => {visitor.visit_this(self)}
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ExprStmt(Expr),
    PrintStmt(Expr),
    ReturnStmt(Token, Expr),
    VarStmt(Token, Expr),
    Block(Vec<Stmt>),
    Class(Token, Vec<Stmt>),
    IfStmt(Expr, Box<Stmt>, Option<Box<Stmt>>),
    WhileStmt(Expr, Box<Stmt>),
    Function(Token, Vec<Token>, Vec<Stmt>),
}

impl Stmt {
    pub fn accept<E>(&self, visitor: &mut dyn StmtVisitor<Err=E>) -> Result<(), E> {
        match self {
            Stmt::ExprStmt(_) => { visitor.visit_expr_stmt(self) }
            Stmt::PrintStmt(_) => { visitor.visit_print_stmt(self) }
            Stmt::VarStmt(_, _) => { visitor.visit_var_stmt(self) }
            Stmt::Block(_) => { visitor.visit_block_stmt(self) }
            Stmt::IfStmt(_, _, _) => { visitor.visit_if_stmt(self) }
            Stmt::WhileStmt(_, _) => { visitor.visit_while_stmt(self) }
            Stmt::Function(_, _, _) => { visitor.visit_func_stmt(self) }
            Stmt::ReturnStmt(_, _) => { visitor.visit_ret_stmt(self) }
            Stmt::Class(_, _) => { visitor.visit_class_stmt(self) }
        }
    }

    pub fn accept_mut<E>(&mut self, visitor: &mut dyn StmtVisitorMut<Err=E>) -> Result<(), E> {
        match self {
            Stmt::ExprStmt(_) => { visitor.visit_expr_stmt(self) }
            Stmt::PrintStmt(_) => { visitor.visit_print_stmt(self) }
            Stmt::VarStmt(_, _) => { visitor.visit_var_stmt(self) }
            Stmt::Block(_) => { visitor.visit_block_stmt(self) }
            Stmt::IfStmt(_, _, _) => { visitor.visit_if_stmt(self) }
            Stmt::WhileStmt(_, _) => { visitor.visit_while_stmt(self) }
            Stmt::Function(_, _, _) => { visitor.visit_func_stmt(self) }
            Stmt::ReturnStmt(_, _) => { visitor.visit_ret_stmt(self) }
            Stmt::Class(_, _) => { visitor.visit_class_stmt(self) }

        }
    }
}




use crate::error::ParserError::*;
use crate::error::ParserError;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

type StmtResult = Result<Stmt, ParserError>;

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

    fn declaration(&mut self) -> StmtResult {
        if self.matches(vec![VAR]) {
            return self.var_declaration();
        }
        if self.matches(vec![FUN]) {
            return self.function("function");
        }
        if self.matches(vec![CLASS]) {
            return self.class_declaration();
        }
        self.statement()
    }

    fn class_declaration(&mut self) -> StmtResult {
        let name = self.expect_id("Expect class name.".to_string())?.clone();
        self.expect(&LEFT_BRACE, "Expect '{' before class body.".to_string())?;

        let mut methods = vec![];

        while !self.check(&RIGHT_BRACE) && !self.is_at_end() {
            let method = self.function("method")?;
            methods.push(method)
        }

        self.expect(&RIGHT_BRACE, "Expect '}' after class body.".to_string())?;

        Ok(Stmt::Class(name, methods))
    }

    fn function(&mut self, kind: &'static str) -> StmtResult {
        let name =
            self.expect_id(format!("Expect {} name.", kind))?.clone();

        self.expect(&LEFT_PAREN, "Expect '(' after function name.".to_string())?;

        let mut params = vec![];
        while !self.matches(vec![RIGHT_PAREN]) {
            let param = self.expect_id(String::from("Expect parameter name."))?;
            params.push(param.clone());
            self.matches(vec![COMMA]);
        }
        self.expect(&LEFT_BRACE,
                    format!("Expect '{{' before {} body.", kind))?;

        let body = self.block()?;

        Ok(Stmt::Function(name, params, body))
    }


    fn var_declaration(&mut self) -> StmtResult {
        let token = self.peek();
        if let IDENTIFIER(ref _name) = token.token_type {
            let name = token.clone();
            self.advance();
            let initializer: Expr = if self.check(&EQUAL) {
                self.advance();
                self.expression()?
            } else {
                Expr::Literal(Token::new(NIL, name.pos.clone()))
            };
            self.consume(&SEMICOLON, |line| ExpectSemi(line))?;
            return Ok(Stmt::VarStmt(name, initializer));
        }
        Err(ExpectVarName(token.pos.line))
    }

    fn statement(&mut self) -> StmtResult {
        if self.matches(vec![PRINT]) {
            return self.print_stmt();
        }
        if self.matches(vec![LEFT_BRACE]) {
            return self.block_stmt();
        }
        if self.matches(vec![IF]) {
            return self.if_statement();
        }
        if self.matches(vec![WHILE]) {
            return self.while_statement();
        }
        if self.matches(vec![FOR]) {
            return self.for_statement();
        }
        if self.matches(vec![RETURN]) {
            return self.return_statement();
        }
        self.expr_stmt()
    }

    fn return_statement(&mut self) -> StmtResult {
        let token = self.previous().clone();
        let value: Expr = if !self.check(&SEMICOLON) {
            self.expression()?
        } else {
            Expr::Literal(Token { token_type: NIL, pos: token.pos.clone() })
        };

        self.expect(&SEMICOLON, "Expect ';' after return.".to_string())?;
        Ok(Stmt::ReturnStmt(token, value))
    }

    fn for_statement(&mut self) -> StmtResult {
        self.consume(&LEFT_PAREN, |line| ExpectForParen(line))?;
        let initializer = match self.peek().token_type {
            SEMICOLON => {
                self.advance();
                None
            }
            VAR => {
                self.advance();
                Some(self.var_declaration()?)
            }
            _ => { Some(self.expr_stmt()?) }
        };
        let condition = if !self.check(&SEMICOLON) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&SEMICOLON, |line| ExpectError {
            msg: String::from("Expect ';' after loop condition."),
            line,
        })?;
        let increment = if !self.check(&RIGHT_PAREN) {
            Some(self.expression()?)
        } else { None };

        self.consume(&RIGHT_PAREN, |line| ExpectError {
            msg: String::from("Expect ')' after for clauses."),
            line,
        })?;
        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::ExprStmt(increment)]);
        }
        body = Stmt::WhileStmt(condition.unwrap_or(Expr::Literal(Token {
            token_type: TRUE,
            pos: Default::default(),
        })), Box::new(body));

        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn while_statement(&mut self) -> StmtResult {
        self.consume(&LEFT_PAREN, |line| ExpectWhileParen(line))?;
        let condition = self.expression()?;
        self.consume(&RIGHT_PAREN, |line| ExpectWhileParen(line))?;
        let body = self.statement()?;

        Ok(Stmt::WhileStmt(condition, Box::new(body)))
    }

    fn if_statement(&mut self) -> StmtResult {
        self.consume(&LEFT_PAREN, |line| ExpectIfParen(line))?;
        let condition = self.expression()?;
        self.consume(&RIGHT_PAREN, |line| ExpectIfParen(line))?;
        let then_branch = self.statement()?;
        let mut else_branch: Option<Box<Stmt>> = None;
        if self.matches(vec![ELSE]) {
            else_branch = Some(Box::new(self.statement()?));
        }
        Ok(Stmt::IfStmt(condition, Box::new(then_branch), else_branch))
    }

    fn block_stmt(&mut self) -> StmtResult {
        let stmts = self.block()?;
        Ok(Stmt::Block(stmts))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = vec![];
        while !self.check(&RIGHT_BRACE) && !self.is_at_end() {
            let stmt = self.declaration()?;
            stmts.push(stmt);
        }
        self.consume(&RIGHT_BRACE, |line| ExpectRightBrace(line))?;
        Ok(stmts)
    }


    fn print_stmt(&mut self) -> StmtResult {
        let expr = self.expression()?;
        self.consume(&SEMICOLON, |line| ExpectSemi(line))?;
        Ok(Stmt::PrintStmt(expr))
    }

    fn expr_stmt(&mut self) -> StmtResult {
        let expr = self.expression()?;
        self.consume(&SEMICOLON, |line| ExpectSemi(line))?;
        Ok(Stmt::ExprStmt(expr))
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || self.peek().token_type == EOF
    }

    pub fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParserError> {
        let expr = self.or()?;
        if self.matches(vec![EQUAL]) {
            let equals = self.previous();
            let line = equals.pos.line;
            let value = self.assignment()?;
            return match expr {
                Expr::Variable(t,_) => {
                    Ok(Expr::Assign(t, Box::new(value), -1))
                }
                Expr::Get(obj, name) => {
                    Ok(Expr::Set(obj, name, Box::new(value)))
                }
                _ => {
                    Err(ParserError::InvalidAssign(line))
                }
            };
        }
        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.and()?;
        while self.matches(vec![OR]) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.equality()?;
        while self.matches(vec![AND]) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
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

        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;
        loop {
            if self.matches(vec![LEFT_PAREN]) {
                expr = self.finish_call(expr)?;
            } else if self.matches(vec![DOT]) {
                let name =
                    self.expect_id("Expect property name after '.'.".to_string())?;
                expr = Expr::Get(Box::new(expr), name.clone());
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParserError> {
        let mut arguments = vec![];

        if !self.check(&RIGHT_PAREN) {
            loop {
                arguments.push(self.expression()?);
                if arguments.len() > 255 {
                    return Err(ArgumentsExceedError(self.peek().pos.line));
                }
                if !self.matches(vec![COMMA]) {
                    break;
                }
            }
        }
        let paren = self.consume(&RIGHT_PAREN,
                                 |line| ExpectError {
                                     msg: "Expect ')' after arguments.".to_string(),
                                     line,
                                 })?;
        Ok(Expr::Call(Box::new(callee), paren.clone(), Box::new(arguments)))
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        let t = self.peek();

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
                             |line| ParserError::ExpectRightParen(line))?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            IDENTIFIER(_) => {
                let token = t.clone();
                self.advance();
                Ok(Expr::Variable(token, -1))
            }
            THIS => {
                let token = t.clone();
                self.advance();
                Ok(Expr::This(token, -1))
            }
            _ => {
                let line = self.peek().pos.line;
                Err(ParserError::ExpectExpr(line))
            }
        }
    }
    fn consume<F>(&mut self, token_type: &TokenType, on_err: F) -> Result<&Token, ParserError>
        where
            F: FnOnce(usize) -> ParserError,
    {
        if self.check(token_type) {
            self.advance();
            Ok(self.previous())
        } else {
            let line = self.peek().pos.line;
            Err(on_err(line))
        }
    }
    fn expect_id(&mut self, msg: String) -> Result<&Token, ParserError> {
        let t = self.peek();
        if let IDENTIFIER(_name) = &t.token_type {
            self.advance();
            Ok(self.previous())
        } else {
            Err(ExpectError {
                msg,
                line:t.pos.line
            })
        }
    }

    fn expect(&mut self, token_type: &TokenType, msg: String) -> Result<&Token, ParserError> {
        self.consume(token_type, |line| ExpectError {
            msg,
            line
        })
    }
}

#[cfg(test)]
mod test {
    use crate::ast_printer::AstPrinter;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::error::LoxError;

    #[test]
    fn test_expr() -> Result<(), LoxError> {
        let source = "-1 + 2 * 3/(6-5)";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.expression()?;
        let mut printer = AstPrinter::new();
        assert_eq!(
            printer.print(&expr),
            "(+ (- 1) (/ (* 2 3) (group (- 6 5))))"
        );
        Ok(())
    }
}
