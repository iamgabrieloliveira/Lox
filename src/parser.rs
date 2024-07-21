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
    match kind {
        TokenType::BangEqual | TokenType::EqualEqual => true,
        _ => false,
    }
}

fn is_comparison(kind: TokenType) -> bool {
    match kind {
        TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual => {
            true
        }
        _ => false,
    }
}

fn is_term(kind: TokenType) -> bool {
    match kind {
        TokenType::Minus | TokenType::Plus => true,
        _ => false,
    }
}

fn is_factor(kind: TokenType) -> bool {
    match kind {
        TokenType::Star | TokenType::Slash => true,
        _ => false,
    }
}

fn is_unary(kind: TokenType) -> bool {
    match kind {
        TokenType::Bang | TokenType::Minus => true,
        _ => false,
    }
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
        let expr = self.comparison();

        match expr {
            Ok(mut expr) => {
                while is_equality(self.peek().kind) {
                    self.advance();

                    let operator = self.previous().clone();

                    let right = self.comparison();

                    match right {
                        Ok(right) => {
                            expr = Expression::Binary {
                                left: Box::new(expr),
                                operator,
                                right: Box::new(right),
                            };
                        }
                        Err(err) => return Err(err),
                    };
                }

                Ok(expr)
            }
            Err(err) => Err(err),
        }
    }

    fn comparison(&mut self) -> ParserResult<Expression<'a>> {
        let expr = self.term();

        match expr {
            Ok(mut expr) => {
                while is_comparison(self.peek().kind) {
                    self.advance();

                    let operator = self.previous().clone();

                    let right = self.term();

                    match right {
                        Ok(right) => {
                            expr = Expression::Binary {
                                left: Box::new(expr),
                                operator,
                                right: Box::new(right),
                            };
                        }
                        Err(err) => return Err(err),
                    };
                }

                Ok(expr)
            }
            Err(err) => Err(err),
        }
    }

    fn term(&mut self) -> ParserResult<Expression<'a>> {
        let expr = self.factor();

        match expr {
            Ok(mut expr) => {
                while is_term(self.peek().kind) {
                    self.advance();

                    let operator = self.previous().clone();

                    let right = self.factor();

                    match right {
                        Ok(right) => {
                            expr = Expression::Binary {
                                left: Box::new(expr),
                                operator,
                                right: Box::new(right),
                            };
                        }
                        Err(err) => return Err(err),
                    };
                }

                Ok(expr)
            }
            Err(err) => Err(err),
        }
    }

    fn factor(&mut self) -> ParserResult<Expression<'a>> {
        let expr = self.unary();

        match expr {
            Ok(mut expr) => {
                while is_factor(self.peek().kind) {
                    self.advance();

                    let operator = self.previous().clone();

                    let right = self.unary();

                    match right {
                        Ok(right) => {
                            expr = Expression::Binary {
                                left: Box::new(expr),
                                operator,
                                right: Box::new(right),
                            };
                        }
                        Err(err) => return Err(err),
                    };
                }

                Ok(expr)
            }
            Err(err) => Err(err),
        }
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
            TokenType::Number => {
                self.advance();

                let token = self.previous();
                let value = token.literal.clone();
                let value = value.unwrap().parse::<f64>().expect("Invalid number");
                let literal = Literal::Number(value);

                Ok(Expression::Literal(literal))
            }
            TokenType::String => {
                self.advance();

                let token = self.previous();
                let value = token.literal.clone();
                let value = value.unwrap();
                let literal = Literal::String(value);

                Ok(Expression::Literal(literal))
            }
            TokenType::LeftParen => {
                self.advance();

                let expr = self.expression();

                let right_paren =
                    self.consume::<()>(TokenType::RightParen, "Expect ')' after expression");

                match right_paren {
                    Err(err) => Err(err),
                    Ok(_) => match expr {
                        Ok(expr) => Ok(Expression::Grouping(Box::new(expr))),
                        Err(err) => Err(err),
                    },
                }
            }
            _ => {
                let token = self.peek().clone();
                Self::error(token, "Expect expression".to_string())
            }
        }
    }

    fn consume<T>(&mut self, kind: TokenType, message: &str) -> ParserResult<&Token<'a>> {
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

        assert_eq!(expression, Expression::Literal(Literal::Number(1.0)));
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
                    literal: None,
                    line: 1
                },
                right: Box::new(Expression::Literal(Literal::Number(1.0)))
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
                left: Box::new(Expression::Literal(Literal::Number(1.0))),
                operator: Token {
                    kind: TokenType::Plus,
                    lexeme: "+",
                    literal: None,
                    line: 1
                },
                right: Box::new(Expression::Literal(Literal::Number(2.0)))
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
                left: Box::new(Expression::Literal(Literal::Number(1.0))),
                operator: Token {
                    kind: TokenType::Plus,
                    lexeme: "+",
                    literal: None,
                    line: 1
                },
                right: Box::new(Expression::Literal(Literal::Number(2.0)))
            }))
        );
    }
}
