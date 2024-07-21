use std::str::FromStr;

use crate::lox::Lox;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
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

impl FromStr for TokenType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        return match s {
            "and" => Ok(TokenType::And),
            "class" => Ok(TokenType::Class),
            "else" => Ok(TokenType::Else),
            "false" => Ok(TokenType::False),
            "true" => Ok(TokenType::True),
            "for" => Ok(TokenType::For),
            "fun" => Ok(TokenType::Fun),
            "if" => Ok(TokenType::If),
            "nil" => Ok(TokenType::Nil),
            "or" => Ok(TokenType::Or),
            "print" => Ok(TokenType::Print),
            "return" => Ok(TokenType::Return),
            "super" => Ok(TokenType::Super),
            "this" => Ok(TokenType::This),
            "var" => Ok(TokenType::Var),
            "while" => Ok(TokenType::While),
            _ => Err(()),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenType,
    pub lexeme: &'a str,
    pub literal: Option<String>,
    pub line: u32,
}

pub struct Lexer<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            start: usize::default(),
            current: usize::default(),
            line: 1,
        }
    }

    fn advance(&mut self) -> Option<String> {
        let char = self.char_at(self.current);
        self.current += 1;
        char
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        if c.is_none() {
            return;
        }

        let c = c.unwrap();

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
            }
            "=" => {
                let token = if self.matches("=") {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };

                self.add_token(token, None);
            }
            "<" => {
                let token = if self.matches("=") {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };

                self.add_token(token, None);
            }
            ">" => {
                let token = if self.matches("=") {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };

                self.add_token(token, None);
            }
            "/" => {
                if self.matches("/") {
                    while self.peek().ne("\n") && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash, None);
                }
            }
            " " | "\r" | "\t" => {}
            "\n" => {
                self.line += 1;
            }
            "\"" => self.read_string(),
            _ => {
                let char = c.chars().next().unwrap();

                if char.is_numeric() {
                    self.read_number();
                }

                if char.is_alphabetic() {
                    self.read_identifier();
                }
            }
        }
    }

    fn current_slice(&self) -> &str {
        return &self.source[self.start..self.current];
    }

    fn read_identifier(&mut self) {
        while self.peek_char().is_alphabetic() {
            self.advance();
        }

        let text = self.current_slice();

        match TokenType::from_str(text) {
            Ok(kind) => self.add_token(kind, None),
            Err(_) => self.add_token(TokenType::Identifier, None),
        };
    }

    fn is_digit(&mut self, char: String) -> bool {
        let char = char.chars().next().unwrap();
        char.is_numeric()
    }

    fn read_number(&mut self) {
        loop {
            let value = self.peek();

            if !self.is_digit(value) {
                break;
            }

            self.advance();
        }

        let next = self.peek_next().unwrap();

        if self.peek() == "." && self.is_digit(next) {
            self.advance();

            loop {
                let value = self.peek();

                if !self.is_digit(value) {
                    break;
                }

                self.advance();
            }
        }

        let value = self.current_slice();

        let value = value.parse::<i32>().unwrap();
        // todo: find a way to add the value without converting to string
        self.add_token(TokenType::Number, Some(value.to_string()));
    }

    fn peek_next(&mut self) -> Option<String> {
        if self.current + 1 >= self.source.len() {
            return Some(String::from("\0"));
        }

        return self.char_at(self.current + 1);
    }

    fn read_string(&mut self) {
        while self.peek().ne("\"") && !self.is_at_end() {
            // for multiline strings
            if self.peek().eq("\n") {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            self.error("Unterminated String");
        }

        self.advance();
        // trim the surrounding quotes
        let value = &self.source[self.start + 1..self.current - 1];

        self.add_token(TokenType::String, Some(value.to_string()));
    }

    fn peek_char(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        return self.current_char();
    }

    fn current_char(&mut self) -> char {
        self.source.chars().nth(self.current).unwrap()
    }

    fn peek(&mut self) -> String {
        if self.is_at_end() {
            return String::from("\0");
        }

        return self.current_token().unwrap_or(String::from("\0"));
    }

    fn char_at(&self, index: usize) -> Option<String> {
        let char = self.source.chars().nth(index);

        match char {
            Some(c) => Some(c.to_string()),
            None => None,
        }
    }

    fn current_token(&mut self) -> Option<String> {
        self.char_at(self.current)
    }

    fn matches(&mut self, expected: &str) -> bool {
        if self.is_at_end() {
            return false;
        }

        match self.current_token() {
            Some(token) => {
                if token != expected {
                    return false;
                }

                self.current += 1;
                return true;
            }
            None => {
                return false;
            }
        }
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

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.add_token(TokenType::Eof, None);

        return self.tokens.clone();
    }

    fn is_at_end(&self) -> bool {
        &self.current >= &self.source.len()
    }

    pub fn print_tokens(&mut self) {
        dbg!(&self.tokens);
    }

    fn error(&mut self, message: &str) {
        Lox::report(self.line, "", message);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn equal_equal() {
        let mut lexer = Lexer::new("1 == 1");

        let result = lexer.scan_tokens();
        let mut tokens = result.iter();

        let first = tokens.next().unwrap();

        assert_eq!(first.kind, TokenType::Number);
        assert_eq!(first.line, 1);
        assert_eq!(first.lexeme, "1");
        assert_eq!(first.literal, Some(String::from("1")));

        let second = tokens.next().unwrap();

        assert_eq!(second.kind, TokenType::EqualEqual);
        assert_eq!(second.line, 1);
        assert_eq!(second.lexeme, "==");
        assert_eq!(second.literal, None);
    }

    #[test]
    fn bang_equal() {
        let mut lexer = Lexer::new("1 != 1");

        let result = lexer.scan_tokens();
        let mut tokens = result.iter();

        let first = tokens.next().unwrap();

        assert_eq!(first.kind, TokenType::Number);
        assert_eq!(first.line, 1);
        assert_eq!(first.lexeme, "1");
        assert_eq!(first.literal, Some(String::from("1")));

        let second = tokens.next().unwrap();

        assert_eq!(second.kind, TokenType::BangEqual);
        assert_eq!(second.line, 1);
        assert_eq!(second.lexeme, "!=");
        assert_eq!(second.literal, None);
    }

    #[test]
    fn greater() {
        let mut lexer = Lexer::new("1 > 1");

        let result = lexer.scan_tokens();
        let mut tokens = result.iter();

        let first = tokens.next().unwrap();

        assert_eq!(first.kind, TokenType::Number);
        assert_eq!(first.line, 1);
        assert_eq!(first.lexeme, "1");
        assert_eq!(first.literal, Some(String::from("1")));

        let second = tokens.next().unwrap();

        assert_eq!(second.kind, TokenType::Greater);
        assert_eq!(second.line, 1);
        assert_eq!(second.lexeme, ">");
        assert_eq!(second.literal, None);
    }

    #[test]
    fn greater_equal() {
        let mut lexer = Lexer::new("1 >= 1");

        let result = lexer.scan_tokens();
        let mut tokens = result.iter();

        let first = tokens.next().unwrap();

        assert_eq!(first.kind, TokenType::Number);
        assert_eq!(first.line, 1);
        assert_eq!(first.lexeme, "1");
        assert_eq!(first.literal, Some(String::from("1")));

        let second = tokens.next().unwrap();

        assert_eq!(second.kind, TokenType::GreaterEqual);
        assert_eq!(second.line, 1);
        assert_eq!(second.lexeme, ">=");
        assert_eq!(second.literal, None);
    }

    #[test]
    fn less() {
        let mut lexer = Lexer::new("1 < 1");

        let result = lexer.scan_tokens();
        let mut tokens = result.iter();

        let first = tokens.next().unwrap();

        assert_eq!(first.kind, TokenType::Number);
        assert_eq!(first.line, 1);
        assert_eq!(first.lexeme, "1");
        assert_eq!(first.literal, Some(String::from("1")));

        let second = tokens.next().unwrap();

        assert_eq!(second.kind, TokenType::Less);
        assert_eq!(second.line, 1);
        assert_eq!(second.lexeme, "<");
        assert_eq!(second.literal, None);
    }

    #[test]
    fn less_equal() {
        let mut lexer = Lexer::new("1 <= 1");

        let result = lexer.scan_tokens();
        let mut tokens = result.iter();

        let first = tokens.next().unwrap();

        assert_eq!(first.kind, TokenType::Number);
        assert_eq!(first.line, 1);
        assert_eq!(first.lexeme, "1");
        assert_eq!(first.literal, Some(String::from("1")));

        let second = tokens.next().unwrap();

        assert_eq!(second.kind, TokenType::LessEqual);
        assert_eq!(second.line, 1);
        assert_eq!(second.lexeme, "<=");
        assert_eq!(second.literal, None);
    }

    #[test]
    fn string() {
        let mut lexer = Lexer::new("\"hello\"");

        let result = lexer.scan_tokens();
        let mut tokens = result.iter();

        let first = tokens.next().unwrap();

        assert_eq!(first.kind, TokenType::String);
        assert_eq!(first.line, 1);
        assert_eq!(first.lexeme, "\"hello\"");
        assert_eq!(first.literal, Some(String::from("hello")));
    }

    #[test]
    fn number() {
        let mut lexer = Lexer::new("1");

        let result = lexer.scan_tokens();
        let mut tokens = result.iter();

        let first = tokens.next().unwrap();

        assert_eq!(first.kind, TokenType::Number);
        assert_eq!(first.line, 1);
        assert_eq!(first.lexeme, "1");
        assert_eq!(first.literal, Some(String::from("1")));
    }

    #[test]
    fn identifier() {
        let mut lexer = Lexer::new("hello");

        let result = lexer.scan_tokens();
        let mut tokens = result.iter();

        let first = tokens.next().unwrap();

        assert_eq!(first.kind, TokenType::Identifier);
        assert_eq!(first.line, 1);
        assert_eq!(first.lexeme, "hello");
        assert_eq!(first.literal, None);
    }

    #[test]
    fn should_skip_comments() {
        let mut lexer = Lexer::new(
            "// this is a comment
            1",
        );

        let result = lexer.scan_tokens();
        let mut tokens = result.iter();

        let first = tokens.next().unwrap();

        assert_eq!(first.kind, TokenType::Number);
        assert_eq!(first.line, 2);
        assert_eq!(first.lexeme, "1");
        assert_eq!(first.literal, Some(String::from("1")));
    }
}
