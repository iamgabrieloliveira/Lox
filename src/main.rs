mod ast;
mod lexer;
mod lox;
mod parser;

use ast::Expression;
use clap::Parser;
use lexer::{Token, TokenType};
use std::path::PathBuf;

#[derive(Parser, Debug)]
struct Args {
    file: PathBuf,
}

fn main() {}
