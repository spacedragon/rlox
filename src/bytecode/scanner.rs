use num_enum::{IntoPrimitive, TryFromPrimitive};
#[repr(u8)]
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, PartialEq, TryFromPrimitive, IntoPrimitive)]
pub enum TokenType {
    // Single-character tokens.                         
    LEFT_PAREN, RIGHT_PAREN,
    LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS,
    SEMICOLON, SLASH, STAR,

    // One or two character tokens.                     
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,

    // Literals.                                        
    IDENTIFIER, STRING, NUMBER,

    // Keywords.                                        
    AND, CLASS, ELSE, FALSE,
    FOR, FUN, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS,
    TRUE, VAR, WHILE,

    ERROR,
    EOF
}

fn get_keyword(str: &str) -> Option<TokenType> {
    match str {
    "and" =>   Some(AND),
    "class" =>  Some(CLASS),
    "else" =>   Some(ELSE),
    "false" =>  Some(FALSE),
    "for" =>   Some(FOR),
    "fun" =>   Some(FUN),
    "if"  =>   Some(IF),
    "nil" =>   Some(NIL),
    "or"  =>   Some(OR),
    "print" =>  Some(PRINT),
    "return" => Some(RETURN),
    "super" =>  Some(SUPER),
    "this" =>   Some(THIS),
    "true" =>   Some(TRUE),
    "var" =>   Some(VAR),
    "while" =>  Some(WHILE),
        _ => None
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let v:u8 = self.clone().into();
        write!(f, "{}", v)
    }
}


use TokenType::*;
use crate::error::ScannerError::*;
use crate::error::ScannerError;
use std::fmt::{Formatter, Display};


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pos {
    pub start: usize,
    pub len: usize,
    pub line: usize,
}

impl Default for Pos {
    fn default() -> Self {
        Pos {
            line:1,
            start:0,
            len:0
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub t: TokenType,
    pub pos: Pos,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            t: EOF,
            pos: Default::default()
        }
    }
}

impl Token {
    pub fn new(t: TokenType, pos: Pos) -> Self {
        Token {
            t,
            pos
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.t)
    }
}

pub struct Scanner {
    source: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Scanner {
            source: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn init(&mut self) {
        self.start = 0;
        self.current = 0;
        self.line = 1;
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }

    pub fn scan_token(&mut self) -> Result<Token, ScannerError> {
        self.skip_whitespace();
        self.start = self.current;
        if self.is_at_end() {
            return Ok(self.make_token(EOF));
        }

        let c = self.advance();
        let token = match c {
            '(' => self.make_token(LEFT_PAREN),
            ')' => self.make_token(RIGHT_PAREN),
            '{' => self.make_token(LEFT_BRACE),
            '}' => self.make_token(RIGHT_BRACE),
            ',' => self.make_token(COMMA),
            '.' => self.make_token(DOT),
            '-' => self.make_token(MINUS),
            '+' => self.make_token(PLUS),
            ';' => self.make_token(SEMICOLON),
            '*' => self.make_token(STAR),
            '!' => {
                let t = if self.matches('=') { BANG_EQUAL } else { BANG };
                self.make_token(t)
            }
            '=' => {
                let t = if self.matches('=') { EQUAL_EQUAL } else { EQUAL };
                self.make_token(t)
            }
            '<' => {
                let t = if self.matches('=') { LESS_EQUAL } else { LESS };
                self.make_token(t)
            }
            '>' => {
                let t = if self.matches('=') { GREATER_EQUAL } else { GREATER };
                self.make_token(t)
            }
            '"' => self.string()?,
            '0'..='9' => self.number()?,
            '/' => self.make_token(SLASH),
            _ if c.is_alphabetic() || c == '_' => {
                self.identifier()
            }
            _ => {
                return Result::Err(UnexpectedCharacter(c,self.line));
            }
        };
        Result::Ok(token)
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                },
                '\n' => {
                    self.advance();
                    self.line += 1
                },
                '/' => {
                    if self.matches('/') {
                        // A comment goes until the end of the line.
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return
                    }
                }
                _ => {
                    return
                }
            }
        }
    }

    fn string(&mut self) -> Result<Token, ScannerError> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            return Result::Err(UnterminatedString(self.line));
        }
        self.advance();

        Ok(self.make_token(STRING))
    }

    fn number(&mut self) -> Result<Token, ScannerError> {
        while self.peek().is_digit(10) {
            self.advance();
        }
        if self.peek() == '.' {
            self.advance();
        }
        while self.peek().is_digit(10) {
            self.advance();
        }


        Ok(self.make_token(NUMBER))
    }

    fn identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric() {
            self.advance();
        }
        let value_str: String = self.source[self.start..self.current].iter().collect();
        if let Some(k) = get_keyword(&value_str) {
            self.make_token(k.clone())
        } else {
            self.make_token(IDENTIFIER)
        }
    }



    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source[self.current]
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source[self.current] != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn make_token(&mut self, token_type: TokenType) -> Token {
        Token {
            t: token_type,
            pos: Pos {
                start: self.start,
                line: self.line,
                len: self.current - self.start,
            },
        }
    }

    pub fn get_identifier(&self, token: &Token) -> String {
        let Pos { start, len, ..} = &token.pos;
        let chars = &self.source[(*start)..(start+len)];
        let result: String = chars.iter().collect();
        result
    }

    pub fn identifier_eq(&self, a: &Token, b: &Token) -> bool {
        let a_range = a.pos.start..(a.pos.start+a.pos.len) ;
        let b_range = b.pos.start..(b.pos.start+b.pos.len) ;
        return self.source[a_range] == self.source[b_range]
    }

    pub fn get_string(&self, token: &Token) -> String {
        let Pos { start, len, ..} = &token.pos;
        let chars = &self.source[(start+1)..(start+len -1)];
        let result: String = chars.iter().collect();
        result
    }

    pub fn get_number(&self, token: &Token) -> f64 {
        let Pos { start, len, ..} = &token.pos;
        let chars = &self.source[(*start)..(start+len)];
        let result: String = chars.iter().collect();

        let value: f64 = result.parse()
            .expect(format!("can't parse number from '{}'", result).as_str());
        value
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_token_code() {
        let code: u8 = LEFT_PAREN.into();
        assert_eq!(code, 0u8);
        let code: u8 = EOF.into();
        assert_eq!(code, 39u8);
        let code: u8 = BANG.into();
        assert_eq!(code, 11u8);
    }

    #[test]
    fn test_string() {
        let mut scanner = Scanner::new(r#""aaa""#);
        let token = scanner.scan_token().unwrap();
        assert_eq!(token.t, STRING);
        assert_eq!(token.pos.start, 0);
        assert_eq!(token.pos.len, 5);
        assert_eq!(scanner.get_string(&token), "aaa".to_string());
    }

    #[test]
    fn test_number() {
        let mut scanner = Scanner::new(r#" 256.128 "#);
        let token = scanner.scan_token().unwrap();
        assert_eq!(token.t, NUMBER);
        assert_eq!(token.pos.start, 1);
        assert_eq!(token.pos.len, 7);
        assert_eq!(scanner.get_number(&token), 256.128f64);
    }

}