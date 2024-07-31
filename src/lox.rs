use crate::lexer::{Token, TokenType};

#[derive(Debug)]
pub struct Lox;

impl Lox {
    pub fn error(token: Token, message: String) {
        if token.kind == TokenType::Eof {
            Self::report(token.line, " at end", &message);
        } else {
            let place = format!(" at '{}'", token.lexeme);
            Self::report(token.line, place.as_str(), &message);
        }
    }

    pub fn report(line: u32, place: &str, message: &str) {
        panic!("[Line: {}] Error{}: {}", line, place, message);
    }
}
