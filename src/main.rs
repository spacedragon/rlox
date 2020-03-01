#![allow(dead_code)]
#![allow(unreachable_patterns)]

use structopt::StructOpt;
use log::{info};
use std::path::PathBuf;
use crate::value::Value;
use crate::scanner::Scanner;
use crate::parser::Parser;
use crate::interpreter::Interpreter;

use crate::error::LoxError;
use std::fs;
use crate::resolver::Resolver;
use crate::environment::clock;
use crate::output::Output;
use std::io::stdout;

mod scanner;
mod parser;
mod ast_printer;
mod interpreter;
mod environment;
mod value;
mod resolver;
mod error;
mod bytecode;
mod output;

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
    },
    Test {

    }
}



fn fib(n:i64) -> i64 {
    if n<2 {
        return n;
    }
    fib(n -1) + fib(n-2)
}


use crate::bytecode::vm::VM;

fn main() -> Result<(), LoxError>{
    env_logger::init();
    info!("starting up");
    let matches = Opt::from_args();
    match &matches.cmd {
        Command::Compile{ files }  => {
            for file in files {
                let source = fs::read_to_string(file)?;
                let mut vm = VM::new();
                vm.interpret(&source);
            }
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
                let output = stdout();
                let mut interpreter = Interpreter::new(output);

                interpreter.interpret(&stmts)?;
            }
        }
        Command:: Test {} => {
            if let Value::NUMBER(before) = clock(vec![]) {
                println!("{}", fib(35));
                if let Value::NUMBER(after) = clock(vec![]) {
                    println!("{}", after - before);
                }
            }
        }
    }
    Ok(())
}
