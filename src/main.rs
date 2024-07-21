mod ast;
mod lexer;
mod lox;
mod parser;

use clap::Parser;
use lexer::Token;
use std::path::PathBuf;

#[derive(Parser, Debug)]
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

    dbg!(expression);
}
