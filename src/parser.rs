use crate::{
    ast::{Expression, Literal},
    lexer::TokenType,
    lox::Lox,
    Token,
};

pub type ParserResult<T> = std::result::Result<T, ParserError>;

#[derive(Debug, Clone)]
pub struct ParserError;

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
}

fn is_equality(kind: TokenType) -> bool {
    matches!(kind, TokenType::BangEqual | TokenType::EqualEqual)
}

fn is_comparison(kind: TokenType) -> bool {
    matches!(
        kind,
        TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual
    )
}

fn is_term(kind: TokenType) -> bool {
    matches!(kind, TokenType::Minus | TokenType::Plus)
}

fn is_factor(kind: TokenType) -> bool {
    matches!(kind, TokenType::Slash | TokenType::Star)
}

fn is_unary(kind: TokenType) -> bool {
    matches!(kind, TokenType::Bang | TokenType::Minus)
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> ParserResult<Expression<'a>> {
        self.expression()
    }

    fn expression(&mut self) -> ParserResult<Expression<'a>> {
        self.equality()
    }

    fn equality(&mut self) -> ParserResult<Expression<'a>> {
        let mut expr = self.comparison()?;

        while is_equality(self.peek().kind) {
            self.advance();

            let operator = self.previous().clone();

            let right = self.comparison()?;

            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParserResult<Expression<'a>> {
        let mut expr = self.term()?;

        while is_comparison(self.peek().kind) {
            self.advance();

            let operator = self.previous().clone();

            let right = self.term()?;

            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> ParserResult<Expression<'a>> {
        let mut expr = self.factor()?;

        while is_term(self.peek().kind) {
            self.advance();

            let operator = self.previous().clone();

            let right = self.factor()?;

            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ParserResult<Expression<'a>> {
        let mut expr = self.unary()?;

        while is_factor(self.peek().kind) {
            self.advance();

            let operator = self.previous().clone();

            let right = self.unary()?;

            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParserResult<Expression<'a>> {
        if !is_unary(self.peek().kind) {
            return self.primary();
        }

        self.advance();

        let operator = self.previous().clone();
        let right = self.unary();

        right.map(|right| Expression::Unary {
            operator,
            right: Box::new(right),
        })
    }

    fn primary(&mut self) -> ParserResult<Expression<'a>> {
        match self.peek().kind {
            TokenType::True => {
                self.advance();
                Ok(Expression::Literal(Literal::Boolean(true)))
            }
            TokenType::False => {
                self.advance();
                Ok(Expression::Literal(Literal::Boolean(false)))
            }
            TokenType::Nil => {
                self.advance();
                Ok(Expression::Literal(Literal::Nil))
            }
            TokenType::Integer => {
                self.advance();

                let token = self.previous();

                let value = token.literal.clone();

                match value {
                    crate::lexer::Literal::Integer(value) => {
                        Ok(Expression::Literal(Literal::Integer(value)))
                    }
                    _ => {
                        // todo: A good way to handle/prevent it?
                        panic!("Token with type Integer cannot contains a non-integer literal type")
                    }
                }
            }
            TokenType::Float => {
                self.advance();

                let token = self.previous();
                let value = token.literal.clone();

                match value {
                    crate::lexer::Literal::Float(value) => {
                        Ok(Expression::Literal(Literal::Float(value)))
                    }
                    _ => {
                        // todo: A good way to handle/prevent it?
                        panic!("Token with type Float cannot contains a non-float literal type")
                    }
                }
            }
            TokenType::String => {
                self.advance();

                let token = self.previous();
                let value = token.literal.clone();

                match value {
                    crate::lexer::Literal::String(value) => {
                        Ok(Expression::Literal(Literal::String(value)))
                    }
                    _ => {
                        // todo: A good way to handle/prevent it?
                        panic!("Token with type String cannot contains a non-string literal type")
                    }
                }
            }
            TokenType::LeftParen => {
                self.advance();

                let expr = self.expression()?;

                self.consume(TokenType::RightParen, "Expect ')' after expression")?;

                Ok(Expression::Grouping(Box::new(expr)))
            }
            _ => {
                let token = self.peek().clone();
                Self::error(token, "Expect expression".to_string())
            }
        }
    }

    fn consume(&mut self, kind: TokenType, message: &str) -> ParserResult<&Token<'a>> {
        let token = self.peek().clone();

        if token.kind == kind {
            return Ok(self.advance());
        }

        Self::error(token, message.to_string())
    }

    fn error<T>(token: Token, message: String) -> ParserResult<T> {
        Lox::error(token, message);

        Err(ParserError)
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_end() {
            if self.previous().kind == TokenType::Semicolon {
                return;
            }

            match self.peek().kind {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {
                    self.advance();
                }
            };
        }
    }

    fn peek(&self) -> &Token<'a> {
        &self.tokens[self.current]
    }

    fn check(&self, kind: TokenType) -> bool {
        if self.is_end() {
            return false;
        }

        return self.peek().kind == kind;
    }

    fn previous(&self) -> &Token<'a> {
        if self.current == 0 {
            return &self.tokens[0];
        }

        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token<'a> {
        if !self.is_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn is_end(&self) -> bool {
        self.peek().kind == TokenType::Eof
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn literal() {
        let source = String::from("1");

        let mut lexer = Lexer::new(&source);

        let tokens = lexer.scan_tokens();

        let mut parser = Parser::new(tokens);

        let expression = parser.parse().unwrap();

        assert_eq!(expression, Expression::Literal(Literal::Integer(1)));
    }

    #[test]
    fn unary() {
        let source = String::from("-1");

        let mut lexer = Lexer::new(&source);

        let tokens = lexer.scan_tokens();

        let mut parser = Parser::new(tokens);

        let expression = parser.parse().unwrap();

        assert_eq!(
            expression,
            Expression::Unary {
                operator: Token {
                    kind: TokenType::Minus,
                    lexeme: "-",
                    literal: crate::lexer::Literal::None,
                    line: 1
                },
                right: Box::new(Expression::Literal(Literal::Integer(1)))
            }
        );
    }

    #[test]
    fn binary() {
        let source = String::from("1 + 2");
        let mut lexer = Lexer::new(&source);
        let tokens = lexer.scan_tokens();

        let mut parser = Parser::new(tokens);

        let expression = parser.parse().unwrap();

        assert_eq!(
            expression,
            Expression::Binary {
                left: Box::new(Expression::Literal(Literal::Integer(1))),
                operator: Token {
                    kind: TokenType::Plus,
                    lexeme: "+",
                    literal: crate::lexer::Literal::None,
                    line: 1
                },
                right: Box::new(Expression::Literal(Literal::Integer(2)))
            }
        );
    }

    #[test]
    fn grouping() {
        let source = String::from("(1 + 2)");
        let mut lexer = Lexer::new(&source);
        let tokens = lexer.scan_tokens();

        let mut parser = Parser::new(tokens);

        let expression = parser.parse().unwrap();

        assert_eq!(
            expression,
            Expression::Grouping(Box::new(Expression::Binary {
                left: Box::new(Expression::Literal(Literal::Integer(1))),
                operator: Token {
                    kind: TokenType::Plus,
                    lexeme: "+",
                    literal: crate::lexer::Literal::None,
                    line: 1
                },
                right: Box::new(Expression::Literal(Literal::Integer(2)))
            }))
        );
    }
}
