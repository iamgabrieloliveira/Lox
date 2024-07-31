use crate::{
    ast::{Expression, Literal, Statement},
    environment::Environment,
    lexer::{Token, TokenType},
};

pub type InterpreterResult<'a, T> = std::result::Result<T, InterpreterError<'a>>;

#[derive(Debug, Clone)]
pub struct InterpreterError<'a> {
    pub token: Token<'a>,
    pub message: &'a str,
}

pub struct Interpreter<'a> {
    environment: Environment<'a>,
    statements: Vec<Statement<'a>>,
}

impl<'a> Interpreter<'a> {
    pub fn new(statements: Vec<Statement<'a>>) -> Self {
        Self {
            environment: Environment::new(),
            statements,
        }
    }

    pub fn interpret(&mut self) -> Result<(), InterpreterError<'a>> {
        // todo: remove that clone
        for statement in &self.statements.clone() {
            self.execute(statement)?;
        }

        Ok(())
    }

    pub fn execute(&mut self, statement: &Statement<'a>) -> Result<(), InterpreterError<'a>> {
        match statement {
            Statement::Expression(expression) => self.evaluate(expression).map(|_| ()),
            Statement::Print(expression) => {
                let value = self.evaluate(expression)?;
                println!("{}", value);
                Ok(())
            }
            Statement::Var { name, expression } => {
                match expression {
                    Some(expr) => {
                        let value = self.evaluate(expr).unwrap();
                        self.environment.define(name.lexeme, Some(value));
                    }
                    None => {
                        self.environment.define(name.lexeme, None);
                    }
                };

                Ok(())
            }
        }
    }

    pub fn evaluate(&self, expr: &Expression<'a>) -> InterpreterResult<'a, Literal> {
        match expr {
            Expression::Literal(literal) => Ok(literal.clone()),
            Expression::Grouping(literal) => self.evaluate(&literal),
            Expression::Unary { operator, right } => self.evaluate_unary(operator, right),
            Expression::Binary {
                left,
                operator,
                right,
            } => self.evaluate_binary(left, &operator, right),
            Expression::Variable(token) => {
                let value = self.environment.get(token.lexeme);

                match value {
                    Some(value) => match value {
                        Some(value) => Ok(value.clone()),
                        None => Ok(Literal::Nil),
                    },
                    None => Err(InterpreterError {
                        token: token.clone(),
                        message: "Undefined variable",
                    }),
                }
            }
        }
    }

    pub fn evaluate_unary(
        &self,
        operator: &Token<'a>,
        right: &Expression<'a>,
    ) -> InterpreterResult<'a, Literal> {
        let value = self.evaluate(&right)?;

        match operator.kind {
            TokenType::Bang => Ok(Literal::Boolean(!self.is_truthy(&value))),
            TokenType::Minus => match value {
                Literal::Integer(i) => Ok(Literal::Integer(-i)),
                Literal::Float(f) => Ok(Literal::Float(-f)),
                _ => Err(InterpreterError {
                    token: operator.clone(),
                    message: "Operand must be a number",
                }),
            },
            _ => unreachable!(),
        }
    }

    fn evaluate_binary(
        &self,
        left: &Expression<'a>,
        operator: &Token<'a>,
        right: &Expression<'a>,
    ) -> InterpreterResult<'a, Literal> {
        let left = self.evaluate(&left)?;
        let right = self.evaluate(&right)?;

        match (operator.kind, left, right) {
            (TokenType::Plus, Literal::Integer(left), Literal::Integer(right)) => {
                Ok(Literal::Integer(left + right))
            }
            (TokenType::Plus, Literal::String(mut left), Literal::String(right)) => {
                left.push_str(right.as_str());
                Ok(Literal::String(left))
            }
            (TokenType::Minus, Literal::Integer(left), Literal::Integer(right)) => {
                Ok(Literal::Integer(left - right))
            }
            (TokenType::Minus, Literal::Float(left), Literal::Float(right)) => {
                Ok(Literal::Float(left - right))
            }
            (TokenType::Greater, Literal::Integer(left), Literal::Integer(right)) => {
                Ok(Literal::Boolean(left > right))
            }
            (TokenType::GreaterEqual, Literal::Integer(left), Literal::Integer(right)) => {
                Ok(Literal::Boolean(left >= right))
            }
            (TokenType::Less, Literal::Integer(left), Literal::Integer(right)) => {
                Ok(Literal::Boolean(left < right))
            }
            (TokenType::LessEqual, Literal::Integer(left), Literal::Integer(right)) => {
                Ok(Literal::Boolean(left <= right))
            }
            (TokenType::Slash, Literal::Integer(left), Literal::Integer(right)) => {
                Ok(Literal::Integer(left / right))
            }
            (TokenType::Slash, Literal::Float(left), Literal::Float(right)) => {
                Ok(Literal::Float(left / right))
            }
            (TokenType::Star, Literal::Integer(left), Literal::Integer(right)) => {
                Ok(Literal::Integer(left * right))
            }
            (TokenType::Star, Literal::Float(left), Literal::Float(right)) => {
                Ok(Literal::Float(left * right))
            }
            (
                TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual
                | TokenType::Minus
                | TokenType::Plus
                | TokenType::Slash
                | TokenType::Star,
                _,
                _,
            ) => Err(InterpreterError {
                token: operator.clone(),
                message: "Operands must be numbers",
            }),
            (TokenType::EqualEqual, left, right) => Ok(Literal::Boolean(left.eq(&right))),
            (TokenType::BangEqual, left, right) => Ok(Literal::Boolean(!left.eq(&right))),
            _ => unreachable!(),
        }
    }

    pub fn is_truthy(&self, literal: &Literal) -> bool {
        match literal {
            Literal::Nil => false,
            Literal::Boolean(b) => *b,
            _ => true,
        }
    }
}
