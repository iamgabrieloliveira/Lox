use crate::{
    ast::{Expression, FunctionStatement, Literal, Statement},
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
    matches!(kind, TokenType::Slash | TokenType::Star | TokenType::Module)
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
        let result = {
            match self.peek().kind {
                TokenType::Var => {
                    self.advance();
                    self.variable_declaration()
                }
                TokenType::Fun => {
                    self.advance();
                    self.function_declaration("function")
                }
                _ => self.statement(),
            }
        };

        if result.is_err() {
            self.synchronize();
        }

        result
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
        )?;

        Ok(Statement::Var {
            name: token,
            expression,
        })
    }

    fn statement(&mut self) -> ParserResult<Statement<'a>> {
        let token = self.peek();

        match token.kind {
            TokenType::Break => {
                self.advance();

                self.consume(TokenType::Semicolon, "Expect ';' after break.")?;

                Ok(Statement::Break)
            }
            TokenType::Print => {
                self.advance();
                self.print_statement()
            }
            TokenType::For => {
                self.advance();
                self.for_statement()
            }
            TokenType::If => {
                self.advance();
                self.if_statement()
            }
            TokenType::While => {
                self.advance();
                self.parse_while()
            }
            TokenType::LeftBrace => {
                self.advance();
                self.block()
            }
            _ => self.expression_statement(),
        }
    }

    fn parse_while(&mut self) -> ParserResult<Statement<'a>> {
        self.consume(TokenType::LeftParen, "Expect '(' after while.")?;

        let condition = self.expression()?;

        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let body = self.statement()?;

        Ok(Statement::While {
            condition,
            body: Box::new(body),
        })
    }

    fn if_statement(&mut self) -> ParserResult<Statement<'a>> {
        self.consume(TokenType::LeftParen, "Expect '(' after if.")?;

        let condition = self.expression()?;

        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = self.statement()?;

        let otherwise_branch = match self.peek().kind {
            TokenType::Else => {
                self.advance();
                Some(self.statement()?)
            }
            _ => None,
        };

        return Ok(Statement::If {
            condition,
            then: Box::new(then_branch),
            otherwise: Box::new(otherwise_branch),
        });
    }

    fn block(&mut self) -> ParserResult<Statement<'a>> {
        let mut statements = Vec::new();

        while self.peek().kind != TokenType::RightBrace && !self.is_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;

        Ok(Statement::Block(statements))
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
        self.assignment()
    }

    fn assignment(&mut self) -> ParserResult<Expression<'a>> {
        let expr = self.or()?;

        if self.peek().kind == TokenType::Equal {
            let equals = self.advance().clone();

            let value = self.assignment()?;

            match expr {
                Expression::Variable(token) => {
                    return Ok(Expression::Assign(token, Box::new(value)));
                }
                _ => Self::error(equals, String::from("Invalid assignment target"))?,
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> ParserResult<Expression<'a>> {
        let mut expr = self.and()?;

        while self.peek().kind == TokenType::Or {
            let operator = self.advance().clone();
            let right = self.equality()?;

            expr = Expression::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn and(&mut self) -> ParserResult<Expression<'a>> {
        let mut expr = self.equality()?;

        while self.peek().kind == TokenType::And {
            let operator = self.advance().clone();
            let right = self.equality()?;

            expr = Expression::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
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
            return self.call();
        }

        self.advance();

        let operator = self.previous().clone();
        let right = self.unary();

        right.map(|right| Expression::Unary {
            operator,
            right: Box::new(right),
        })
    }

    fn call(&mut self) -> ParserResult<Expression<'a>> {
        let mut expr = self.primary()?;

        loop {
            let token = self.peek().kind;

            if token == TokenType::LeftParen {
                self.advance();
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expression<'a>) -> ParserResult<Expression<'a>> {
        let mut args: Vec<Expression> = Vec::new();

        if self.peek().kind != TokenType::RightParen {
            loop {
                args.push(self.expression()?);

                if self.peek().kind != TokenType::Comma {
                    break;
                } else {
                    self.advance();
                }
            }
        }

        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;

        return Ok(Expression::Call {
            callee: Box::new(callee),
            paren: paren.clone(),
            arguments: args,
        });
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

    fn synchronize(&mut self) -> () {
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
                _ => self.advance(),
            };
        }
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

    fn for_statement(&mut self) -> Result<Statement<'a>, ParserError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let token = self.advance().kind;

        let initializer = match token {
            // If we match the semicolon,
            // it means that we don't have a initializer:
            // e.g: for (;i < 10; i++) {}
            TokenType::Semicolon => None,
            TokenType::Var => Some(self.variable_declaration()?),
            _ => Some(self.expression_statement()?),
        };

        let token = self.peek().kind;

        let condition = match token {
            TokenType::Semicolon => None,
            _ => Some(self.expression()?),
        };

        self.consume(TokenType::Semicolon, "Expect ')' after loop condition.")?;

        let token = self.peek().kind;

        let increment = match token {
            TokenType::Semicolon => None,
            _ => Some(self.expression()?),
        };

        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        body = match increment {
            Some(inc) => Statement::Block(vec![body, Statement::Expression(inc)]),
            None => body,
        };

        match condition {
            None => {
                body = Statement::While {
                    condition: Expression::Literal(Literal::Boolean(true)),
                    body: Box::new(body),
                }
            }
            Some(cond) => {
                body = Statement::While {
                    condition: cond,
                    body: Box::new(body),
                }
            }
        };

        body = match initializer {
            Some(init) => Statement::Block(vec![init, body]),
            None => body,
        };

        Ok(body)
    }

    fn function_declaration(&mut self, kind: &str) -> Result<Statement<'a>, ParserError> {
        let name = self
            .consume(
                TokenType::Identifier,
                format!("Expected {} name.", kind).as_str(),
            )?
            .clone();

        self.consume(
            TokenType::LeftParen,
            format!("Expected '(' after {} name.", kind).as_str(),
        )?;

        let token = self.peek().kind;

        let mut params: Vec<Token> = Vec::with_capacity(255);

        // if it's not a right paren it means that we should have arguments in that function
        if token != TokenType::RightParen {
            loop {
                if params.len() >= 255 {
                    Self::error(
                        self.peek().clone(),
                        "Cannot have more than 255 parameters.".to_string(),
                    )?;
                }

                let param = self.consume(TokenType::Identifier, "Expected parameter name.")?;

                params.push(param.clone());

                let token = self.peek().kind;

                if token != TokenType::Comma {
                    break;
                }

                self.advance();
            }
        }

        self.consume(TokenType::RightParen, "Expected ')' after parameters.")?;
        self.consume(
            TokenType::LeftBrace,
            format!("Expected '{{' before {} body.", kind).as_str(),
        )?;

        let body = self.block()?;

        match body {
            Statement::Block(sttms) => {
                let r#fn = Statement::Function(FunctionStatement {
                    name,
                    params,
                    body: sttms,
                });

                Ok(r#fn)
            }
            _ => unreachable!(),
        }
    }
}
