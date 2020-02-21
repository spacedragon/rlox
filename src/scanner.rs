use failure::{Fail};
use lazy_static::lazy_static;
use strum_macros::{Display};

#[derive(Debug, Fail)]
pub enum ScannerError {
    #[fail(display = "Unexpected character {} on line {}.", ch, line)]
    UnexpectedCharacter {
        ch: char,
        line: usize,
    },
    #[fail(display = "Unterminated string on line {}.", line)]
    UnterminatedString {
        line: usize
    },
    #[fail(display = "Invalid Number string {} on line {}.", str, line)]
    InvalidNumber {
        line: usize,
        str: String,
    },
}

#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq, Display)]
pub enum TokenType {
    // Single-character tokens.
    #[strum(serialize="(")]
    LEFT_PAREN,
    #[strum(serialize=")")]
    RIGHT_PAREN,
    #[strum(serialize="{")]
    LEFT_BRACE,
    #[strum(serialize="}")]
    RIGHT_BRACE,
    #[strum(serialize=",")]
    COMMA,
    #[strum(serialize=".")]
    DOT,
    #[strum(serialize="-")]
    MINUS,
    #[strum(serialize="+")]
    PLUS,
    #[strum(serialize=";")]
    SEMICOLON,
    #[strum(serialize="/")]
    SLASH,
    #[strum(serialize="*")]
    STAR,

    // One or two character tokens.
    #[strum(serialize="!")]
    BANG,
    #[strum(serialize="!=")]
    BANG_EQUAL,
    #[strum(serialize="=")]
    EQUAL,
    #[strum(serialize="==")]
    EQUAL_EQUAL,
    #[strum(serialize=">")]
    GREATER,
    #[strum(serialize=">=")]
    GREATER_EQUAL,
    #[strum(serialize="<")]
    LESS,
    #[strum(serialize="<=")]
    LESS_EQUAL,

    // Literals.
    IDENTIFIER(String),
    STRING(String),
    NUMBER(f64),

    // Keywords.
    #[strum(serialize="and")]
    AND,
    #[strum(serialize="class")]
    CLASS,
    #[strum(serialize="else")]
    ELSE,
    #[strum(serialize="false")]
    FALSE,
    #[strum(serialize="fun")]
    FUN,
    #[strum(serialize="for")]
    FOR,
    #[strum(serialize="if")]
    IF,
    #[strum(serialize="nil")]
    NIL,
    #[strum(serialize="or")]
    OR,
    #[strum(serialize="print")]
    PRINT,
    #[strum(serialize="return")]
    RETURN,
    #[strum(serialize="super")]
    SUPER,
    #[strum(serialize="this")]
    THIS,
    #[strum(serialize="true")]
    TRUE,
    #[strum(serialize="var")]
    VAR,
    #[strum(serialize="while")]
    WHILE,

    EOF,
}

impl TokenType {}

use TokenType::*;
use crate::scanner::ScannerError::{UnexpectedCharacter, UnterminatedString, InvalidNumber};
use std::collections::HashMap;
use failure::_core::fmt::{Formatter};

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("and",    AND);
        m.insert("class",  CLASS);
        m.insert("else",   ELSE);
        m.insert("false",  FALSE);
        m.insert("for",    FOR);
        m.insert("fun",    FUN);
        m.insert("if",     IF);
        m.insert("nil",    NIL);
        m.insert("or",     OR);
        m.insert("print",  PRINT);
        m.insert("return", RETURN);
        m.insert("super",  SUPER);
        m.insert("this",   THIS);
        m.insert("true",   TRUE);
        m.insert("var",    VAR);
        m.insert("while",  WHILE);
        m
    };
}


#[derive(Clone, Debug, PartialEq)]
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
    pub token_type: TokenType,
    pub pos: Pos,
}

impl Token {
    pub fn new(token_type: TokenType, pos: Pos) -> Self {
        Token {
            token_type,
            pos
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_type)
    }
}

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Scanner {
            source: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, ScannerError> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }
        self.tokens.push(Token {
            token_type: EOF,
            pos: Pos {
                line: self.line,
                start: self.start,
                len: 0,
            },
        });
        Result::Ok(self.tokens)
    }

    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }

    fn scan_token(&mut self) -> Result<(), ScannerError> {
        let c = self.advance();
        match c {
            '(' => self.add_token(LEFT_PAREN),
            ')' => self.add_token(RIGHT_PAREN),
            '{' => self.add_token(LEFT_BRACE),
            '}' => self.add_token(RIGHT_BRACE),
            ',' => self.add_token(COMMA),
            '.' => self.add_token(DOT),
            '-' => self.add_token(MINUS),
            '+' => self.add_token(PLUS),
            ';' => self.add_token(SEMICOLON),
            '*' => self.add_token(STAR),
            '!' => {
                let t = if self.matches('=') { BANG_EQUAL } else { BANG };
                self.add_token(t)
            }
            '=' => {
                let t = if self.matches('=') { EQUAL_EQUAL } else { EQUAL };
                self.add_token(t)
            }
            '<' => {
                let t = if self.matches('=') { LESS_EQUAL } else { LESS };
                self.add_token(t)
            }
            '>' => {
                let t = if self.matches('=') { GREATER_EQUAL } else { GREATER };
                self.add_token(t)
            }
            '/' => {
                if self.matches('/') {
                    // A comment goes until the end of the line.
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(SLASH);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,
            '"' => self.string()?,
            '0'..='9' => {
                self.number()?;
            }
            _ if c.is_alphabetic() || c == '_' => {
                self.identifier();
            }
            _ => {
                return Result::Err(UnexpectedCharacter {
                    ch: c,
                    line: self.line,
                });
            }
        }
        Result::Ok(())
    }

    fn string(&mut self) -> Result<(), ScannerError> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            return Result::Err(UnterminatedString { line: self.line });
        }
        self.advance();

        let value = self.source[self.start + 1..self.current - 1].iter().collect();
        self.add_token(STRING(value));

        Result::Ok(())
    }

    fn number(&mut self) -> Result<(), ScannerError> {
        while self.peek().is_digit(10) {
            self.advance();
        }
        if self.peek() == '.' {
            self.advance();
        }
        while self.peek().is_digit(10) {
            self.advance();
        }
        let value_str: String = self.source[self.start..self.current].iter().collect();
        let value: f64 = value_str.parse().map_err(|_| InvalidNumber {
            str: value_str,
            line: self.line,
        })?;

        self.add_token(NUMBER(value));
        Ok(())
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.advance();
        }
        let value_str: String = self.source[self.start..self.current].iter().collect();
        if let Some(k) = KEYWORDS.get(value_str.as_str()) {
            self.add_token(k.clone());
        } else {
            self.add_token(IDENTIFIER(value_str))
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        return self.source[self.current];
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source[self.current] != expected {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token {
            token_type,
            pos: Pos {
                start: self.start,
                line: self.line,
                len: self.current - self.start + 1,
            },
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn matches(source: &str, expected: Vec<TokenType>) {
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        let result: Vec<TokenType> = tokens.iter().map(|t| t.token_type.clone()).collect();
        assert_eq!(result, expected);
    }



    #[test]
    fn test_single_token() {
        matches("(", vec![LEFT_PAREN, EOF]);
        matches("( )", vec![LEFT_PAREN, RIGHT_PAREN, EOF]);
        matches("()/", vec![LEFT_PAREN, RIGHT_PAREN, SLASH, EOF]);
    }

    #[test]
    fn test_2_tokens() {
        matches(">", vec![GREATER, EOF]);
        matches(">=", vec![GREATER_EQUAL, EOF]);
        matches("==", vec![EQUAL_EQUAL,  EOF]);
    }

    #[test]
    fn test_comment() {
        matches("//abcdef", vec![EOF]);
    }

    #[test]
    fn test_string() {
        matches("\"aaa\"", vec![STRING(String::from("aaa")), EOF]);
    }

    #[test]
    #[should_panic]
    fn test_bad_string() {
        matches("\"aaa", vec![STRING(String::from("aaa")), EOF]);
    }

    #[test]
    fn test_number() {
        matches("12.34", vec![NUMBER(12.34f64), EOF]);
        matches("1234", vec![NUMBER(1234f64), EOF]);
    }

    #[test]
    fn test_id_and_keyword() {
        matches("and", vec![AND, EOF]);
        matches("_avar", vec![IDENTIFIER(String::from("_avar")), EOF]);
    }
}