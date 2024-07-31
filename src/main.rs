mod ast;
mod environment;
mod interpreter;
mod lexer;
mod lox;
mod parser;

use clap::Parser as ArgsParser;
use interpreter::Interpreter;
use lexer::Token;
use std::path::PathBuf;

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

    let mut interpreter = Interpreter::new(expression);

    let result = interpreter.interpret();

    if result.is_err() {
        let error = result.unwrap_err();
        println!("{:?}", error);
        std::process::exit(65);
    }
}
