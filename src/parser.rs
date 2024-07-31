use crate::{
    ast::{Expression, Literal, Statement},
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

    pub fn parse(&mut self) -> ParserResult<Vec<Statement>> {
        let mut statements: Vec<Statement> = Vec::new();

        while !self.is_end() {
            let statement = self.declaration()?;

            statements.push(statement);
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> ParserResult<Statement<'a>> {
        if self.peek().kind == TokenType::Var {
            self.advance();
            return self.variable_declaration();
        } else {
            return self.statement();
        }
    }

    fn variable_declaration(&mut self) -> ParserResult<Statement<'a>> {
        let token = self
            .consume(TokenType::Identifier, "Expect variable name.")?
            .clone();

        let mut expression = None;

        if self.peek().kind == TokenType::Equal {
            self.advance();

            expression = Some(self.expression()?);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )
        .unwrap();

        Ok(Statement::Var {
            name: token,
            expression,
        })
    }

    fn statement(&mut self) -> ParserResult<Statement<'a>> {
        if self.peek().kind == TokenType::Print {
            self.advance();
            return self.print_statement();
        }

        self.expression_statement()
    }

    fn print_statement(&mut self) -> ParserResult<Statement<'a>> {
        let value = self.expression()?;

        self.consume(TokenType::Semicolon, "Expected ';' after value.")?;

        Ok(Statement::Print(value))
    }

    fn expression_statement(&mut self) -> ParserResult<Statement<'a>> {
        let expression = self.expression()?;

        self.consume(TokenType::Semicolon, "Expected ';' after value.")?;

        Ok(Statement::Expression(expression))
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
                let token = self.advance();

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
                let token = self.advance();

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
            TokenType::Identifier => {
                let token = self.advance();
                Ok(Expression::Variable(token.clone()))
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
        let token = self.peek();

        if token.kind == kind {
            return Ok(self.advance());
        }

        Self::error(token.clone(), message.to_string())
    }

    fn error<T>(token: Token, message: String) -> ParserResult<T> {
        Lox::error(token, message);

        Err(ParserError)
    }

    fn peek(&self) -> &Token<'a> {
        &self.tokens[self.current]
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
