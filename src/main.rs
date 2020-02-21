

#![allow(dead_code)]

use structopt::StructOpt;
use log::{info};
use std::path::PathBuf;


mod scanner;
mod parser;
mod ast_printer;
mod interpreter;

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


fn main() {
    env_logger::init();
    info!("starting up");
    let matches = Opt::from_args();
    match &matches.cmd {
        Command::Compile{ files: _ }  => {
            println!("{:?}", matches);

        }
        Command::Run{ files: _ }  => {

        }
    }

}
