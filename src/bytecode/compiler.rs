use super::chunk::Chunk;
use super::scanner::{Scanner, Token, TokenType};
use super::scanner::TokenType::*;
use crate::error::LoxError;
use crate::bytecode::debug::{print_err, disassemble_chunk};
use crate::bytecode::OpCode::*;
use std::mem;
use crate::bytecode::value::Value;

use Precedence::*;
use crate::bytecode::OpCode;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::convert::TryFrom;
use crate::bytecode::memory::ALLOCATOR;
use crate::error::LoxError::CompileError;


struct Local {
    name: Token,
    depth: i32,
}

pub struct Compiler {
    parser: Parser,
    current: FunCompiler,
}

struct FunCompiler {
    scope_depth: usize,
    locals: Vec<Local>,
    function: Value,
    function_type: FunctionType,
    enclosing: Option<Box<FunCompiler>>,
}

impl FunCompiler {
    fn new(function_type: FunctionType) -> Self {
        let function = ALLOCATOR.with(|a| { a.borrow_mut().new_function() });
        Self {
            function: Value::Obj(function),
            function_type,
            scope_depth: 0,
            locals: vec![Local {
                depth: 0,
                name: Token {
                    t: TokenType::IDENTIFIER,
                    pos: Default::default(),
                },
            }],
            enclosing: None,
        }
    }
}

impl Default for FunCompiler {
    fn default() -> Self {
        Self {
            function: Value::Nil,
            function_type: FunctionType::Script,
            scope_depth: 0,
            locals: vec![],
            enclosing: None,
        }
    }
}

#[derive(Clone)]
enum FunctionType {
    Function,
    Script,
}

struct Parser {
    scanner: Scanner,
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
}

impl Parser {
    fn new(source: &str) -> Self {
        Self {
            scanner: Scanner::new(source),
            previous: Default::default(),
            current: Default::default(),
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn advance(&mut self) {
        mem::replace(&mut self.previous, self.current.clone());

        loop {
            match self.scanner.scan_token() {
                Ok(t) => {
                    mem::replace(&mut self.current, t);
                    return;
                }
                Err(e) => {
                    self.error(format!("{}", e).as_str())
                }
            }
        }
    }


    fn error_at_current(&mut self, e: &str) {
        let token = self.current.clone();
        self.error_at(&token, e);
    }

    fn error_at(&mut self, token: &Token, e: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        print_err(format!("[line {}]", token.pos.line));
        match token.t {
            EOF => {
                print_err(" at end".to_string())
            }
            _ => {
                print_err(format!(" at '{}.{}'", token.pos.len, token.pos.start));
            }
        }
        print_err(format!("{}\n", e));

        self.had_error = true;
    }

    fn error(&mut self, e: &str) {
        let token = self.previous.clone();
        self.error_at(&token, e)
    }
}

impl Compiler {
    pub fn new(source: &str) -> Self {
        Self {
            parser: Parser::new(source),
            current: FunCompiler::new(FunctionType::Script),
        }
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        let f = self.current.function.as_function();
        &mut f.chunk
    }

    fn init_compiler(&mut self, function_type: FunctionType) {
        let compiler = FunCompiler::new(function_type.clone());
        let previous = mem::replace(&mut self.current, compiler);
        self.current.enclosing = Some(Box::new(previous));
        match function_type {
            FunctionType::Script => {}
            _ => {
                let token = self.parser.previous.clone();
                if let Value::Obj(object) = self.copy_string(&token) {
                    let function = self.current.function.as_function();
                    function.name = object.as_ptr()
                }
            }
        }
    }

    pub fn compile(mut self) -> Result<Value, LoxError> {
        self.parser.had_error = false;
        self.parser.panic_mode = false;

        self.parser.advance();

        while !self.matches(EOF) {
            self.declaration();
        }
        self.consume(EOF, "Expect end of expression.");


        if self.parser.had_error {
            Err(CompileError)
        } else {
            let fun = self.end_compiler();
            Ok(fun)
        }
    }

    fn end_compiler(&mut self) -> Value {
        self.emit_return();
        if !self.parser.had_error {
            #[cfg(debug_assertions)]
                {
                    let function = self.current.function.as_function();
                    let name = function.name().to_string();
                    let chunk = self.current_chunk();
                    disassemble_chunk(chunk, &name)
                }
        }
        let result = self.current.function.clone();
        if self.current.enclosing.is_some() {
            let tmp = mem::take(&mut self.current);
            self.current = *tmp.enclosing.unwrap();
            return tmp.function;
        }
        return result;
    }

    fn declaration(&mut self) {
        if self.matches(FUN) {
            self.fun_declaration();
        } else if self.matches(VAR) {
            self.var_declaration();
        } else {
            self.statement();
        }


        if self.parser.panic_mode {
            self.synchronize()
        }
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_var("Expect function name.");
        self.mark_initialized();
        self.function(FunctionType::Function);
        self.define_var(global);
    }

    fn function(&mut self, function_type: FunctionType) {
        self.init_compiler(function_type);
        self.begin_scope();
        self.consume(LEFT_PAREN, "Expect '(' after function name.");

        if !self.check(RIGHT_PAREN) {
            loop {
                let f = self.current.function.as_function();
                f.arity += 1;
                if f.arity > std::u8::MAX as usize {
                    self.parser.error_at_current("Cannot have more than 255 parameters.");
                }

                let param = self.parse_var("Expect parameter name.");
                self.define_var(param);
                if !self.matches(COMMA) {
                    break;
                }
            }
        }

        self.consume(RIGHT_PAREN, "Expect ')' after parameters.");

        self.consume(LEFT_BRACE, "Expect '{' before function body.");
        self.block();

        let fun = self.end_compiler();
        self.emit_constant(fun);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_var("Expect variable name.");

        if self.matches(EQUAL) {
            self.expression();
        } else {
            self.emit_op(OpNil);
        }
        self.consume(SEMICOLON, "Expect ';' after variable declaration.");

        self.define_var(global);
    }

    fn define_var(&mut self, global: u8) {
        if self.current.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(OpDefineGlobal.into(), global);
    }

    fn argument_list(&mut self) -> u8 {
        let mut arg_count = 0;
        if !self.check(RIGHT_PAREN) {
            loop {
                if arg_count == std::u8::MAX {
                    self.error("Cannot have more than 255 arguments.");
                }

                self.expression();
                arg_count += 1;
                if !self.matches(COMMA) {
                    break;
                }
            }
        }
        self.consume(RIGHT_PAREN, "Expect ')' after arguments.");
        return arg_count;
    }

    fn mark_initialized(&mut self) {
        if self.current.scope_depth == 0 {
            return;
        }
        if let Some(f) = self.current.locals.last_mut() {
            f.depth = self.current.scope_depth as i32;
        }
    }

    fn declare_var(&mut self) {
        if self.current.scope_depth == 0 {
            return;
        }
        let name = (&self.parser.previous).clone();
        let name_conflicts = self.current.locals.iter().any(|local| {
            if (local.depth != -1) && (local.depth < self.current.scope_depth as i32) {
                false
            } else {
                self.parser.scanner.identifier_eq(&local.name, &name)
            }
        });

        if name_conflicts {
            self.parser.error_at_current("Variable with this name already declared in this scope.")
        } else {
            self.add_local(name);
        }
    }

    fn add_local(&mut self, name: Token) {
        if self.current.locals.len() >= std::u8::MAX as usize {
            self.parser.error_at_current("Too many local variables in function.");
            return;
        }

        let local = Local {
            name,
            depth: -1,
        };
        self.current.locals.push(local)
    }

    fn parse_var(&mut self, err: &str) -> u8 {
        self.consume(IDENTIFIER, err);
        let token = self.parser.previous.clone();

        self.declare_var();
        if self.current.scope_depth > 0 {
            return 0;
        }

        self.identifier_constant(&token)
    }

    fn copy_string(&mut self, token: &Token) -> Value {
        ALLOCATOR.with(|a| {
            let chars = self.parser.scanner.get_chars(token);
            a.borrow_mut().allocate_string(chars).into()
        })
    }

    fn identifier_constant(&mut self, name: &Token) -> u8 {
        let string = self.copy_string(name);
        self.make_constant(string)
    }

    fn make_constant(&mut self, v: Value) -> u8 {
        self.current_chunk().add_constant(v)
    }
    fn synchronize(&mut self) {
        self.parser.panic_mode = false;
        while self.parser.current.t != EOF {
            if self.parser.previous.t == SEMICOLON {
                return;
            }

            match self.parser.current.t {
                CLASS |
                FUN |
                VAR |
                FOR |
                IF |
                WHILE |
                PRINT |
                RETURN => return,
                _ => {}
            }

            self.parser.advance();
        }
    }

    fn statement(&mut self) {
        if self.matches(PRINT) {
            self.print_statement();
        } else if self.matches(FOR) {
            self.for_statement();
        } else if self.matches(IF) {
            self.if_statement()
        } else if self.matches(RETURN) {
            self.return_statement();
        } else if self.matches(WHILE) {
            self.while_statement();
        } else if self.matches(LEFT_BRACE) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn return_statement(&mut self) {
        if let FunctionType::Script = self.current.function_type {
            self.error("Cannot return from top-level code.");
        }
        if self.matches(SEMICOLON) {
            self.emit_return();
        } else {
            self.expression();
            self.consume(SEMICOLON, "Expect ';' after return value");
            self.emit_op(OpReturn);
        }
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().len();
        self.consume(LEFT_PAREN, "Expect '(' after 'while'.");
        self.expression();
        self.consume(RIGHT_PAREN, "Expect ')' after condition.");
        let exit_jump = self.emit_jump(OpJumpIfFalse);

        self.emit_op(OpPop);
        self.statement();

        self.emit_loop(loop_start);
        self.patch_jump(exit_jump);
        self.emit_op(OpPop);
    }

    fn emit_loop(&mut self, start: usize) {
        self.emit_op(OpLoop);

        let offset = self.current_chunk().len() - start + 2;
        if offset > (std::u16::MAX as usize) {
            self.error("Loop body too large.")
        }

        let [hi, lo] = (offset as u16).to_le_bytes();

        self.emit_bytes(hi, lo);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(LEFT_PAREN, "Expect '(' after 'for'");
        // initializer
        if self.matches(SEMICOLON) {
            // No initializer.
        } else if self.matches(VAR) {
            self.var_declaration();
        } else {
            self.expression_statement()
        }

        let mut loop_start = self.current_chunk().len();
        // condition expression
        let mut exit_jump = None;
        if !self.matches(SEMICOLON) {
            self.expression();
            self.consume(SEMICOLON, "Expect ';' after loop condition.");

            exit_jump = Some(self.emit_jump(OpJumpIfFalse));
            self.emit_op(OpPop);
        }

        //increment
        if !self.matches(RIGHT_PAREN) {
            let body_jump = self.emit_jump(OpJump);
            let increment_start = self.current_chunk().len();
            self.expression();
            self.emit_op(OpPop);
            self.consume(RIGHT_PAREN, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();

        self.emit_loop(loop_start);

        if let Some(exit) = exit_jump {
            self.patch_jump(exit);
            self.emit_op(OpPop);
        }

        self.end_scope();
    }

    fn if_statement(&mut self) {
        self.consume(LEFT_PAREN, "Expect '(' after 'if'.");
        self.expression();
        self.consume(RIGHT_PAREN, "Expect ')' after condition.");

        let then_jump = self.emit_jump(OpJumpIfFalse);
        self.emit_op(OpPop);
        self.statement();

        let else_jump = self.emit_jump(OpJump);

        self.patch_jump(then_jump);
        self.emit_op(OpPop);

        if self.matches(ELSE) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn emit_jump(&mut self, instruction: OpCode) -> usize {
        self.emit_op(instruction);
        self.emit_bytes(0xff, 0xff);
        self.current_chunk().len() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.current_chunk().len() - offset - 2;
        if jump > std::u16::MAX as usize {
            self.error("Too much code to jump over.");
        }

        let jump = jump as u16;
        let [hi, lo] = jump.to_le_bytes();
        let chunk = self.current_chunk();
        chunk[offset] = hi;
        chunk[offset + 1] = lo;
    }

    fn block(&mut self) {
        while !self.check(RIGHT_BRACE) && !self.check(EOF) {
            self.declaration();
        }
        self.consume(RIGHT_BRACE, "Expect '}' after block.");
    }

    fn begin_scope(&mut self) {
        self.current.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.current.scope_depth -= 1;
        let mut n = 0;
        while !self.current.locals.is_empty() &&
            self.current.locals.last().unwrap().depth > (self.current.scope_depth as i32) {
            self.current.locals.pop();
            n += 1;
        }
        if n == 1 {
            self.emit_op(OpPop);
        } else {
            self.emit_bytes(OpPopN.into(), n);
        }
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(SEMICOLON, "Expect ';' after expression.");
        self.emit_op(OpPop);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(SEMICOLON, "Expect ';' after value.");
        self.emit_op(OpPrint)
    }

    fn matches(&mut self, t: TokenType) -> bool {
        if !self.check(t) {
            return false;
        }
        self.parser.advance();
        true
    }

    fn check(&mut self, t: TokenType) -> bool {
        self.parser.current.t == t
    }


    fn error(&mut self, e: &str) {
        self.parser.error(e)
    }

    fn emit_op(&mut self, op: OpCode) {
        self.emit_byte(op.into())
    }

    fn emit_byte(&mut self, byte: u8) {
        let line = self.parser.previous.pos.line;
        self.current_chunk().write_chunk(byte, line as i32);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }


    fn number(&mut self, _can_assign: bool) {
        if self.parser.previous.t == NUMBER {
            let v = self.parser.scanner.get_number(&self.parser.previous);
            self.emit_constant(Value::Number(v))
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let line = self.parser.previous.pos.line as i32;
        self.current_chunk().write_constant(value, line);
    }

    fn emit_return(&mut self) {
        self.emit_op(OpNil);
        self.emit_byte(OpReturn.into());
    }

    fn expression(&mut self) {
        self.parse_precedence(PREC_ASSIGNMENT);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.parser.advance();

        let precedence = precedence as u8;
        let can_assign = precedence <= (PREC_ASSIGNMENT as u8);

        let prefix_rule = get_rule(self.parser.previous.t).prefix;
        if let Some(prefix_rule) = prefix_rule {
            prefix_rule(self, can_assign)
        } else {
            self.parser.error_at_current("Expect expression.");
        }

        while precedence <= (get_rule(self.parser.current.t).precedence as u8) {
            self.parser.advance();
            if let Some(infix) = get_rule(self.parser.previous.t).infix {
                infix(self, can_assign);
            }
        }

        if can_assign && self.matches(EQUAL) {
            self.parser.error_at_current("Invalid assigment target.")
        }
    }

    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn consume(&mut self, token_type: TokenType, message: &str) {
        if self.parser.current.t == token_type {
            self.parser.advance();
            return;
        }
        self.parser.error_at_current(message);
    }

    fn unary(&mut self, _can_assign: bool) {
        let operator_type = &self.parser.previous.t.clone();

        // Compile the operand.
        self.parse_precedence(PREC_UNARY);

        // Emit the operator instruction.
        match operator_type {
            BANG => self.emit_op(OpNot),
            MINUS => self.emit_op(OpNegate),
            _ => {
                unreachable!()
            }
        }
    }

    fn binary(&mut self, _can_assign: bool) {
        let op_type = &self.parser.previous.t.clone();

        let rule = get_rule(*op_type);
        let pre_num: u8 = rule.precedence.clone().into();
        let higher = Precedence::try_from(pre_num + 1).expect("no higher precedence?");
        self.parse_precedence(higher);
        match op_type {
            PLUS => self.emit_op(OpAdd),
            MINUS => self.emit_op(OpSubtract),
            STAR => self.emit_op(OpMultiply),
            SLASH => self.emit_op(OpDivide),
            BANG_EQUAL => {
                self.emit_op(OpEqual);
                self.emit_op(OpNot)
            }
            EQUAL_EQUAL => self.emit_op(OpEqual),
            GREATER => self.emit_op(OpGreater),
            GREATER_EQUAL => {
                self.emit_op(OpLess);
                self.emit_op(OpNot);
            }
            LESS => self.emit_op(OpLess),
            LESS_EQUAL => {
                self.emit_op(OpGreater);
                self.emit_op(OpNot)
            }
            _ => unreachable!()
        }
    }

    fn call(&mut self, _can_assign: bool) {
        let arg_count = self.argument_list();
        self.emit_bytes(OpCall.into(), arg_count);
    }


    fn literal(&mut self, _can_assign: bool) {
        match self.parser.previous.t {
            FALSE => self.emit_op(OpFalse),
            TRUE => self.emit_op(OpTrue),
            NIL => self.emit_op(OpNil),
            _ => { unreachable!() }
        }
    }

    fn string(&mut self, _can_assign: bool) {
        ALLOCATOR.with(|a| {
            let s = self.parser.scanner.get_chars(&self.parser.previous);
            let value = a.borrow_mut().allocate_string(s).into();
            self.emit_constant(value)
        });
    }

    fn variable(&mut self, can_assign: bool) {
        let name = (&self.parser.previous).clone();
        self.named_var(&name, can_assign)
    }

    fn named_var(&mut self, name: &Token, can_assign: bool) {
        let get: OpCode;
        let set: OpCode;
        let arg: u8;
        if let Some(i) = self.resolve_local(name) {
            get = OpGetLocal;
            set = OpSetLocal;
            arg = i;
        } else {
            arg = self.identifier_constant(name);
            set = OpSetGlobal;
            get = OpGetGlobal;
        }

        if can_assign && self.matches(EQUAL) {
            self.expression();
            self.emit_bytes(set.into(), arg);
        } else {
            self.emit_bytes(get.into(), arg);
        }
    }

    fn resolve_local(&mut self, name: &Token) -> Option<u8> {
        for i in (0..self.current.locals.len()).rev() {
            let local = &self.current.locals[i];
            if self.parser.scanner.identifier_eq(&local.name, name) {
                if local.depth == -1 {
                    self.parser.error_at_current("Cannot read local variable in its own initializer.")
                }
                return Some(i as u8);
            }
        }
        None
    }

    fn and_(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(OpJumpIfFalse);

        self.emit_op(OpPop);
        self.parse_precedence(PREC_AND);

        self.patch_jump(end_jump);
    }

    fn or_(&mut self, _can_assign: bool) {
        let else_jump = self.emit_jump(OpJumpIfFalse);
        let end_jump = self.emit_jump(OpJump);

        self.patch_jump(else_jump);
        self.emit_op(OpPop);

        self.parse_precedence(PREC_OR);

        self.patch_jump(end_jump);
    }
}

fn get_rule(token_type: TokenType) -> &'static ParseRule {
    let idx = token_type as u8;
    &RULES[idx as usize]
}


#[allow(non_camel_case_types)]
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, TryFromPrimitive, IntoPrimitive, Clone, Copy)]
enum Precedence {
    PREC_NONE,
    PREC_ASSIGNMENT,
    // =
    PREC_OR,
    // or
    PREC_AND,
    // and
    PREC_EQUALITY,
    // == !=
    PREC_COMPARISON,
    // < > <= >=
    PREC_TERM,
    // + -
    PREC_FACTOR,
    // * /
    PREC_UNARY,
    // ! -
    PREC_CALL,
    // . ()
    PREC_PRIMARY,
}

type ParseFn = fn(&mut Compiler, bool) -> ();

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

macro_rules! rule_expr {
    ($prefix:expr, $infix:expr, $prec:expr ) => { ParseRule {prefix:$prefix, infix:$infix,  precedence:$prec } }
}

macro_rules! rule {
    (None, None, $prec:expr ) => { rule_expr!(None, None, $prec) };
    ($prefix:ident, None, $prec:expr ) => { rule_expr!(Some(Compiler::$prefix), None, $prec) };
    (None, $infix:ident, $prec:expr ) => { rule_expr!(None, Some(Compiler::$infix), $prec) };
    ($prefix:ident, $infix:ident, $prec:expr ) => { rule_expr!(Some(Compiler::$prefix), Some(Compiler::$infix), $prec) };
}

static RULES: [ParseRule; 40] = [
    rule! { grouping, call, PREC_CALL}, // TOKEN_LEFT_PAREN
    rule! { None, None, PREC_NONE },       // TOKEN_RIGHT_PAREN
    rule! { None,  None,  PREC_NONE },      // TOKEN_LEFT_BRACE
    rule! { None,  None,  PREC_NONE },       // TOKEN_RIGHT_BRACE
    rule! { None,  None,  PREC_NONE },       // TOKEN_COMMA
    rule! { None,  None,  PREC_NONE },       // TOKEN_DOT
    rule! { unary, binary,  PREC_TERM },       // TOKEN_MINUS
    rule! { None,  binary,  PREC_TERM },       // TOKEN_PLUS
    rule! { None,  None,  PREC_NONE },       // TOKEN_SEMICOLON
    rule! { None,  binary,  PREC_FACTOR },     // TOKEN_SLASH
    rule! { None,  binary,  PREC_FACTOR },     // TOKEN_STAR
    rule! { unary,  None,  PREC_NONE },       // TOKEN_BANG
    rule! { None,  binary,  PREC_EQUALITY },       // TOKEN_BANG_EQUAL
    rule! { None,  None,  PREC_NONE },       // TOKEN_EQUAL
    rule! { None,  binary,  PREC_EQUALITY },       // TOKEN_EQUAL_EQUAL
    rule! { None,  binary,  PREC_COMPARISON },       // TOKEN_GREATER
    rule! { None,  binary,  PREC_COMPARISON },       // TOKEN_GREATER_EQUAL
    rule! { None,  binary,  PREC_COMPARISON },       // TOKEN_LESS
    rule! { None,  binary,  PREC_COMPARISON },       // TOKEN_LESS_EQUAL
    rule! { variable,  None,  PREC_NONE },       // TOKEN_IDENTIFIER
    rule! { string,  None,  PREC_NONE },       // TOKEN_STRING
    rule! { number,  None,  PREC_NONE },       // TOKEN_NUMBER
    rule! { None,  and_,  PREC_AND },       // TOKEN_AND
    rule! { None,  None,  PREC_NONE },       // TOKEN_CLASS
    rule! { None,  None,  PREC_NONE },       // TOKEN_ELSE
    rule! { literal,  None,  PREC_NONE },       // TOKEN_FALSE
    rule! { None,  None,  PREC_NONE },       // TOKEN_FOR
    rule! { None,  None,  PREC_NONE },       // TOKEN_FUN
    rule! { None,  None,  PREC_NONE },       // TOKEN_IF
    rule! { literal,  None,  PREC_NONE },       // TOKEN_NIL
    rule! { None,  or_,  PREC_OR },       // TOKEN_OR
    rule! { None,  None,  PREC_NONE },       // TOKEN_PRINT
    rule! { None,  None,  PREC_NONE },       // TOKEN_RETURN
    rule! { None,  None,  PREC_NONE },       // TOKEN_SUPER
    rule! { None,  None,  PREC_NONE },       // TOKEN_THIS
    rule! { literal,  None,  PREC_NONE },       // TOKEN_TRUE
    rule! { None,  None,  PREC_NONE },       // TOKEN_VAR
    rule! { None,  None,  PREC_NONE },       // TOKEN_WHILE
    rule! { None,  None,  PREC_NONE },       // TOKEN_ERROR
    rule! { None,  None,  PREC_NONE },       // TOKEN_EOF
];