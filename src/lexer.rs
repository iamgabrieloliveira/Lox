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
    Module,

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
    Integer,
    Float,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    Break,
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
            "break" => Ok(TokenType::Break),
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
pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenType,
    pub lexeme: &'a str,
    pub literal: Literal,
    pub line: u32,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenType, lexeme: &'a str, literal: Literal, line: u32) -> Self {
        Self {
            kind,
            lexeme,
            literal,
            line,
        }
    }

    pub fn string(literal: &'a str, lexeme: &'a str, line: u32) -> Self {
        Self {
            kind: TokenType::String,
            lexeme,
            literal: Literal::String(literal.to_string()),
            line,
        }
    }
}

pub struct Lexer<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: u32,
}

fn is_valid_identifier(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_numeric(c: char) -> bool {
    // Maybe something else
    c.is_numeric()
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn read(&mut self) {
        let c = self.current_char();

        self.current += 1;

        if c.is_none() {
            return;
        }

        let c = c.unwrap();

        match c {
            // Handle single-character tokens.
            '(' => self.add_symbol(TokenType::LeftParen),
            ')' => self.add_symbol(TokenType::RightParen),
            '{' => self.add_symbol(TokenType::LeftBrace),
            '}' => self.add_symbol(TokenType::RightBrace),
            ',' => self.add_symbol(TokenType::Comma),
            '.' => self.add_symbol(TokenType::Dot),
            '-' => self.add_symbol(TokenType::Minus),
            '+' => self.add_symbol(TokenType::Plus),
            ';' => self.add_symbol(TokenType::Semicolon),
            '*' => self.add_symbol(TokenType::Star),
            '%' => self.add_symbol(TokenType::Module),

            // Handle operators with ambiguous meaning.
            '!' => {
                let token = self.match_next('=', TokenType::BangEqual, TokenType::Bang);
                self.add_symbol(token);
            }
            '=' => {
                let token = self.match_next('=', TokenType::EqualEqual, TokenType::Equal);
                self.add_symbol(token);
            }
            '<' => {
                let token = self.match_next('=', TokenType::LessEqual, TokenType::Less);
                self.add_symbol(token)
            }
            '>' => {
                let token = self.match_next('=', TokenType::GreaterEqual, TokenType::Greater);
                self.add_symbol(token)
            }
            '/' => {
                // If we hit a slash and the next character is also a slash,
                // we are dealing with a comment.
                // So if we do not find a slash, we add a slash token.
                if !self.next_char_is('/') {
                    self.add_symbol(TokenType::Slash);
                }

                // Skip comments.
                while self.peek() != '\n' && self.has_more() {
                    self.current += 1;
                }
            }
            // Ignore whitespace.
            ' ' | '\r' | '\t' => {}
            // If we encounter a newline,
            // increment the line number.
            '\n' => self.line += 1,

            '"' => self.read_string(),

            c if c.is_numeric() => self.read_number(),

            c if is_valid_identifier(c) => self.read_identifier(),

            _ => self.error(format!("Unexpected character. {}", c).as_str()),
        };
    }

    fn read_identifier(&mut self) {
        self.take_while(is_valid_identifier);

        let text = self.current_slice().to_string();

        match TokenType::from_str(&text) {
            Ok(k) => self.add_keyword(k),
            Err(()) => self.add_identifier(&text),
        };
    }

    fn add_identifier(&mut self, text: &str) {
        self.add_token(TokenType::Identifier, Literal::String(text.to_string()));
    }

    fn add_keyword(&mut self, kind: TokenType) {
        self.add_token(kind, Literal::None);
    }

    fn take_while<F>(&mut self, condition: F)
    where
        F: Fn(char) -> bool,
    {
        while condition(self.peek()) {
            self.current += 1;
        }
    }

    fn read_number(&mut self) {
        self.take_while(is_numeric);

        let is_float = self.peek() == '.' && self.peek_next().is_numeric();

        if is_float {
            self.current += 1;
            self.take_while(is_numeric);
        }

        let value = &self.source[self.start..self.current];

        if is_float {
            self.add_float(value);
        } else {
            self.add_integer(value);
        }
    }

    // todo: check scape characters
    fn read_string(&mut self) {
        // Pointer to deal with multi-byte characters.
        let mut i = self.current;

        // Read until the end of the string.
        while self.peek() != '"' && self.has_more() {
            let c = self.peek();

            // If we encounter a newline,
            // increment the line number.
            if c == '\n' {
                self.line += 1;
            }

            self.current += 1;
            i += c.len_utf8();
        }

        // If we reach the end of the string,
        // and we didn't found a closing quote,
        // report an error.
        if self.is_at_end() {
            self.error("Unterminated String");
        }

        self.current += 1;

        // Get the value of the string.
        // We need to remove the quotes that's why we use
        // start + 1 and end - 1.
        let value = &self.source[self.start + 1..i];

        self.add_string(value, i + 1);
    }

    fn char_at(&self, index: usize) -> Option<char> {
        self.source.chars().nth(index)
    }

    fn current_char(&self) -> Option<char> {
        self.char_at(self.current)
    }

    fn next_char(&self) -> Option<char> {
        self.char_at(self.current)
    }

    fn peek(&self) -> char {
        // If the current character is the last one,
        // return a null character.
        return self.current_char().unwrap_or('\0');
    }

    fn peek_next(&mut self) -> char {
        return self.next_char().unwrap_or('\0');
    }

    fn next_char_is(&self, expected: char) -> bool {
        return self.has_more() && self.peek() == expected;
    }

    // Used to handle characters with ambiguous meaning.
    // For example, we can't tell if we are dealing with
    // a Greater or GreaterEqual token.
    // We need to look ahead to determine the correct token.
    fn match_next(&mut self, expected: char, then: TokenType, otherwise: TokenType) -> TokenType {
        if self.next_char_is(expected) {
            self.current += 1;
            return then;
        }

        return otherwise;
    }

    fn add_symbol(&mut self, kind: TokenType) {
        self.add_token(kind, Literal::None);
    }

    fn add_string(&mut self, value: &'a str, end: usize) {
        let text = &self.source[self.start..end];

        let token = Token::string(value, text, self.line);

        self.tokens.push(token);
    }

    fn add_integer(&mut self, str: &str) {
        // todo: check how to handle errors like this
        let value = str.parse::<i64>().unwrap();
        self.add_token(TokenType::Integer, Literal::Integer(value));
    }

    fn add_float(&mut self, str: &str) {
        // todo: check how to handle errors like this
        let value = str.parse::<f64>().unwrap();
        self.add_token(TokenType::Float, Literal::Float(value));
    }

    fn add_token(&mut self, kind: TokenType, literal: Literal) {
        let text = &self.source[self.start..self.current];

        let token = Token::new(kind, text, literal, self.line);

        self.tokens.push(token);
    }

    fn slice(&self, start: usize, end: usize) -> &str {
        return &self.source[start..end];
    }

    fn current_slice(&self) -> &str {
        return self.slice(self.start, self.current);
    }

    pub fn run(mut self) -> Vec<Token<'a>> {
        while self.has_more() {
            self.start = self.current;
            self.read();
        }

        self.add_token(TokenType::Eof, Literal::None);

        return self.tokens;
    }

    fn has_more(&self) -> bool {
        return !self.is_at_end();
    }

    fn is_at_end(&self) -> bool {
        &self.current >= &self.source.len()
    }

    fn error(&mut self, message: &str) {
        Lox::report(self.line, "", message);
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn string() {
        let tokens = Lexer::new(r#""Hello, World!""#).run();

        // 0: "Hello, World!"
        // 1: Eof
        assert_eq!(tokens.len(), 2);

        let token = &tokens[0];
        assert_eq!(token.kind, TokenType::String);
        assert_eq!(token.lexeme, r#""Hello, World!""#);
        assert_eq!(token.literal, Literal::String("Hello, World!".to_string()));
    }

    #[test]
    fn string_multi_byte() {
        let tokens = Lexer::new(r#""ê""#).run();

        assert_eq!(tokens.len(), 2);

        let token = &tokens[0];
        assert_eq!(token.kind, TokenType::String);
        assert_eq!(token.lexeme, r#""ê""#);
        assert_eq!(token.literal, Literal::String("ê".to_string()));
    }

    #[test]
    fn empty_string() {
        let tokens = Lexer::new(r#""""#).run();

        assert_eq!(tokens.len(), 2);

        let token = &tokens[0];
        assert_eq!(token.kind, TokenType::String);
        assert_eq!(token.lexeme, r#""""#);
        assert_eq!(token.literal, Literal::String("".to_string()));
    }

    #[test]
    fn string_with_escape_characters() {
        let tokens = Lexer::new(r#""Hello\nWorld!""#).run();

        assert_eq!(tokens.len(), 2);

        let token = &tokens[0];
        assert_eq!(token.kind, TokenType::String);
        assert_eq!(token.lexeme, r#""Hello\nWorld!""#);
        assert_eq!(token.literal, Literal::String("Hello\\nWorld!".to_string()));
    }

    #[test]
    fn string_with_unicode() {
        let tokens = Lexer::new(r#""Hello, 世界!""#).run();

        let mut tokens = tokens.iter();

        assert_eq!(tokens.len(), 2);

        let first = tokens.next().unwrap();

        assert_eq!(first.kind, TokenType::String);
        assert_eq!(first.lexeme, r#""Hello, 世界!""#);
        assert_eq!(first.literal, Literal::String("Hello, 世界!".to_string()));
    }
}
