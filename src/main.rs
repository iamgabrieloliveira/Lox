mod ast;
mod interpreter;
mod lexer;
mod lox;
mod parser;

use clap::Parser as ArgsParser;
use lox::Lox;
use std::path::PathBuf;

use interpreter::Interpreter;
use lexer::Token;

#[derive(ArgsParser, Debug)]
struct Args {
    file: PathBuf,
}

fn main() {
    let args = Args::parse();

    let source = std::fs::read_to_string(&args.file).expect("Failed to read file");

    let mut lexer = lexer::Lexer::new(&source);

    let tokens = lexer.scan_tokens();

    let mut parser = parser::Parser::new(tokens);

    let expression = parser.parse().unwrap();

    let interpreter = Interpreter {};

    match interpreter.evaluate(expression) {
        Ok(value) => println!("{:?}", value),
        Err(err) => Lox::error(err.token, err.message.to_string()),
    }
}
