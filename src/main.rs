mod ast;
mod lexer;

use ast::{print_expression, Binary, Grouping, Literal, Unary};
use clap::Parser;
use lexer::{Token, TokenType};
use std::path::PathBuf;

#[derive(Parser, Debug)]
struct Args {
    file: PathBuf,
}

fn main() {
    let expression = Binary {
        left: Box::new(Unary {
            operator: Box::new({
                Token {
                    kind: TokenType::Minus,
                    lexeme: "-",
                    literal: None,
                    line: 1,
                }
            }),
            right: Box::new(Literal {
                value: Box::new(Token {
                    kind: TokenType::Number,
                    lexeme: "123",
                    literal: None,
                    line: 1,
                }),
            }),
        }),
        operator: Box::new(Token {
            kind: TokenType::Star,
            lexeme: "*",
            literal: None,
            line: 1,
        }),
        right: Box::new(Grouping {
            expression: Box::new(Literal {
                value: Box::new(Token {
                    kind: TokenType::Number,
                    lexeme: "45.67",
                    literal: None,
                    line: 1,
                }),
            }),
        }),
    };

    print_expression(expression);
}
