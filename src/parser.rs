use crate::{
    ast::{Expression, Literal},
    lexer::TokenType,
    lox::Lox,
    Token,
};

type ParserResult<T> = std::result::Result<T, ParserError>;

#[derive(Debug, Clone)]
struct ParserError;

struct Parser<'a> {
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
    fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, current: 0 }
    }

    fn parse(&mut self) -> ParserResult<Expression<'a>> {
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
            TokenType::True => Ok(Expression::Literal(Literal::Boolean(true))),
            TokenType::False => Ok(Expression::Literal(Literal::Boolean(false))),
            TokenType::Nil => Ok(Expression::Literal(Literal::Nil)),
            TokenType::Number => {
                let token = self.previous();
                let value = token.literal.clone();
                let value = value.unwrap().parse::<f64>().expect("Invalid number");
                let literal = Literal::Number(value);

                Ok(Expression::Literal(literal))
            }
            TokenType::String => {
                let token = self.previous();
                let value = token.literal.clone();
                let value = value.unwrap();
                let literal = Literal::String(value);

                Ok(Expression::Literal(literal))
            }
            TokenType::LeftParen => {
                let expr = self.expression();

                match expr {
                    Ok(expr) => Ok(Expression::Grouping(Box::new(expr))),
                    Err(err) => Err(err),
                }
            }
            _ => todo!(),
        }
    }

    fn consume(&mut self, kind: TokenType, message: &str) -> ParserResult<()> {
        let token = self.peek().clone();

        if token.kind == kind {
            self.advance();
        }

        Self::error(token, message.to_string())
    }

    fn error(token: Token, message: String) -> ParserResult<()> {
        Lox::error(token, message);

        Err(ParserError)
    }

    fn peek(&self) -> &Token<'a> {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token<'a> {
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
