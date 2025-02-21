mod environment;
mod functions;
mod interpreter;
mod lexer;
mod lox;
mod parser;
mod types;

use clap::Parser as ArgsParser;
use lexer::Token;
use std::path::PathBuf;

#[derive(ArgsParser, Debug)]
struct Args {
    file: PathBuf,
}

fn main() {
    let args = Args::parse();

    let source = std::fs::read_to_string(&args.file).expect("Failed to read file");

    let lexer = lexer::Lexer::new(&source);

    let tokens = lexer.run();

    let mut parser = parser::Parser::new(tokens);

    let expression = parser.parse().unwrap();

    match interpreter::run(expression) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Error: {:?}", e);
            std::process::exit(1);
        }
    }
}
