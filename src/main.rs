#![allow(dead_code)]

use structopt::StructOpt;
use log::{info};
use std::path::PathBuf;
use crate::scanner::Scanner;
use crate::parser::Parser;
use crate::interpreter::Interpreter;
use crate::string_writer::StringWriter;
use std::io::{Stdout, Write};
use crate::error::LoxError;
use std::fs;
use crate::resolver::Resolver;

mod scanner;
mod parser;
mod ast_printer;
mod interpreter;
mod string_writer;
mod environment;
mod value;
mod resolver;
mod error;

#[derive(StructOpt, Debug)]
#[structopt(name = "rlox")]
pub struct Opt {
    #[structopt(subcommand)]
    cmd: Command

}

#[derive(StructOpt, Debug)]
enum Command {
    Compile {
        #[structopt(name = "FILE", parse(from_os_str))]
        files: Vec<PathBuf>
    },
    Run {
        #[structopt(name = "FILE", parse(from_os_str))]
        files: Vec<PathBuf>
    }
}

impl StringWriter for Stdout {
    fn write_string(&mut self, s: &str) {
        let mut handle = self.lock();
        handle.write_all(s.as_bytes()).expect("write to stdout failed.");
    }
}


fn main() -> Result<(), LoxError>{
    env_logger::init();
    info!("starting up");
    let matches = Opt::from_args();
    match &matches.cmd {
        Command::Compile{ .. }  => {

        }
        Command::Run{ files }  => {
            if files.is_empty() {
                panic!("no input file.");
            }
            for file in files {
                let source = fs::read_to_string(file)?;
                let scanner = Scanner::new(&source);
                let tokens = scanner.scan_tokens()?;
                let parser = Parser::new(tokens);
                let mut stmts = parser.parse()?;
                let mut resolver = Resolver::new();
                resolver.resolve(&mut stmts);
                let output = std::io::stdout();
                let mut interpreter = Interpreter::new(output);

                interpreter.interpret(&stmts)?;
            }
        }
    }
    Ok(())
}
