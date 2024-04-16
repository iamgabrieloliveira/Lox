use clap::Parser;
use core::panic;
use std::{
    fmt::Display, fs, path::PathBuf
};

#[derive(Parser, Debug)]
struct Args {
    file: PathBuf,
}

#[derive(Debug)]
enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    
    // Literals.
    Identifier,
    String,
    Number,
    
    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}

struct Token<'a> {
    kind: TokenType,
    lexeme: &'a str,
    literal: Option<String>,
    line: usize,
}

struct Lox {
    has_error: bool,
}

struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: usize,
}

impl <'a>Scanner<'a> {
    fn new(source: &'a str) -> Self {
        Self { 
            source,
            tokens: Vec::new(),
            start: usize::default(),
            current: usize::default(),
            line: 1 
        }
    }

    fn advance(&mut self) -> String {
        self.current += 1;
        self.char_at(self.current)
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        
        match c.as_str() {
            "(" => self.add_token(TokenType::LeftParen, None),
            ")" => self.add_token(TokenType::RightParen, None),
            "{" => self.add_token(TokenType::LeftBrace, None),
            "}" => self.add_token(TokenType::RightBrace, None),
            "," => self.add_token(TokenType::Comma, None),
            "." => self.add_token(TokenType::Dot, None),
            "-" => self.add_token(TokenType::Minus, None),
            "+" => self.add_token(TokenType::Plus, None),
            ";" => self.add_token(TokenType::Semicolon, None),
            "*" => self.add_token(TokenType::Star, None),
            "!" => {
                let token = if self.matches("=") {
                    TokenType::BangEqual 
                } else {
                    TokenType::Bang 
                };

                self.add_token(token, None);
            },
            "=" => {
                let token = if self.matches("=") {
                    TokenType::EqualEqual 
                } else {
                    TokenType::Equal 
                };

                self.add_token(token, None);
            },
            "<" => {
                let token = if self.matches("=") {
                    TokenType::LessEqual 
                } else {
                    TokenType::Less
                };

                self.add_token(token, None);
            },
            ">" => {
                let token = if self.matches("=") {
                    TokenType::GreaterEqual 
                } else {
                    TokenType::Greater 
                };

                self.add_token(token, None);
            },
            "/" => {
                if self.matches("/") {
                    while self.peek().ne("\n") && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash, None);
                }
            },
            // @todo: check the whitespaces skip
            "\n" => {
                self.line += 1;
            },
            "\"" => self.read_string(),
            _ => panic!("Unrecognized Token"),
        }

        todo!("Page 49");
    }

    fn read_string(&mut self) {
        while self.peek().ne("\"") {
            if self.peek().eq("\n") {
                self.line += 1;
            }
            
            self.advance();
        }

        if self.is_at_end() {
            // todo: use Lox.error function
            panic!("Unterminated String");
        }

        self.advance();

        // trim the surrounding quotes
        let value = &self.source[self.start + 1..self.current + 1];

        self.add_token(TokenType::String, Some(value.to_string()));
    }

    fn peek(&mut self) -> String {
        if self.is_at_end() {
            return String::from("\0");
        }

        return self.current_token();
    }

    fn char_at(&self, index: usize) -> String {
        self.source.chars().nth(index)
            .expect("Unable to find token at this position").to_string()
    }

    fn current_token(&mut self) -> String {
        self.char_at(self.current)
    }

    fn matches(&mut self, expected: &str) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.current_token().as_str().eq(expected) {
            return false;
        }

        
        self.current += 1;
        return true;
    }

    fn add_token(&mut self, kind: TokenType, literal: Option<String>) {
        let text = &self.source[self.start..self.current];

        let token = Token {
            kind,
            lexeme: text,
            literal: match literal {
                Some(value) => Some(value.to_string()),
                None => None,
            },
            line: self.line,
        };

        self.tokens.push(token);
    }

    fn scan_tokens(&mut self) {
        while !self.is_at_end() {
        }
    }

    fn is_at_end(&self) -> bool {
        &self.current >= &self.source.len()
    }
}

impl Lox {
    pub fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }

    pub fn report(&mut self, line: usize, r#where: &str, message: &str) {
        println!("[Line: {}] Error: {} : {}", line, r#where, message);
        self.has_error = true;
    }
}

fn main() {
    let args = Args::parse();

    let file_content = fs::read_to_string(args.file).unwrap();
}
