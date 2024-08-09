use crate::{
    ast::{Expression, Literal, Statement},
    environment::Environment,
    lexer::{Token, TokenType},
};
use std::ops::Deref;

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

fn is_truthy(literal: &Literal) -> bool {
    match literal {
        Literal::Nil => false,
        Literal::Boolean(b) => *b,
        _ => true,
    }
}

impl<'a> Interpreter<'a> {
    pub fn new(statements: Vec<Statement<'a>>) -> Self {
        Self {
            environment: Environment::empty(),
            statements,
        }
    }

    pub fn interpret(&mut self) -> Result<(), InterpreterError<'a>> {
        // todo: how to remove that clone
        for statement in &self.statements.clone() {
            self.execute(statement)?;
        }

        Ok(())
    }

    pub fn execute(&mut self, statement: &Statement<'a>) -> Result<(), InterpreterError<'a>> {
        match statement {
            Statement::Expression(expression) => self.evaluate(expression).map(|_| ()),
            Statement::Print(expression) => {
                // maybe something else
                let value = self.evaluate(expression)?;

                println!("{}", value);

                Ok(())
            }
            Statement::Var { name, expression } => {
                match expression {
                    Some(expr) => {
                        let value = self.evaluate(expr)?;
                        self.environment.define(name.lexeme, Some(value));
                    }
                    None => {
                        self.environment.define(name.lexeme, None);
                    }
                };

                Ok(())
            }
            Statement::Block(statements) => {
                let block_env = Environment::with_enclosing(self.environment.clone());
                self.execute_block(statements, block_env)?;

                Ok(())
            }
            Statement::If {
                condition,
                then,
                otherwise,
            } => {
                let condition = self.evaluate(condition)?;

                if is_truthy(&condition) {
                    self.execute(then)?;
                } else if let Some(s) = otherwise.deref() {
                    self.execute(s)?;
                }

                Ok(())
            }
        }
    }

    pub fn execute_block(
        &mut self,
        statements: &Vec<Statement<'a>>,
        env: Environment<'a>,
    ) -> Result<(), InterpreterError<'a>> {
        let prev = self.environment.clone();

        // Change the environment temporarly for this block's execution
        self.environment = env;

        for statement in statements {
            let result = self.execute(statement);

            if result.is_err() {
                self.environment = prev;
                return result;
            }
        }

        self.environment = prev;

        Ok(())
    }

    pub fn error<T>(token: Token<'a>, message: &'a str) -> Result<T, InterpreterError<'a>> {
        Err(InterpreterError { token, message })
    }

    pub fn evaluate(&mut self, expr: &Expression<'a>) -> Result<Literal, InterpreterError<'a>> {
        match expr {
            Expression::Literal(l) => Ok(l.clone()),
            Expression::Grouping(literal) => self.evaluate(&literal),
            Expression::Unary { operator, right } => self.evaluate_unary(operator, right),
            Expression::Binary {
                left,
                operator,
                right,
            } => self.evaluate_binary(left, &operator, right),
            Expression::Variable(token) => {
                let mut variable = self.environment.get(token.lexeme);

                if variable.is_none() {
                    variable = self
                        .environment
                        .enclosing
                        .as_mut()
                        .map(|env| env.get(token.lexeme))
                        .flatten();
                }

                match variable {
                    Some(value) => value.clone().map_or_else(|| Ok(Literal::Nil), Ok),
                    None => Self::error(token.clone(), "Undefined variable"),
                }
            }
            Expression::Assign(token, value) => {
                let value = self.evaluate(value)?.clone();

                let mut was_defined = self.environment.define(token.lexeme, Some(value.clone()));

                if was_defined.is_none() {
                    was_defined = self
                        .environment
                        .enclosing
                        .as_mut()
                        .map(|env| env.define(token.lexeme, Some(value.clone())))
                        .flatten();
                }

                if was_defined.is_none() {
                    return Self::error(token.clone(), "Undefined variable");
                }

                Ok(value)
            }
            Expression::Logical {
                left,
                operator,
                right,
            } => match operator.kind {
                TokenType::Or => {
                    let left_value = self.evaluate(left)?;

                    if is_truthy(&left_value) {
                        return Ok(left_value);
                    }

                    Ok(self.evaluate(right)?)
                }
                TokenType::And => {
                    let left_value = self.evaluate(left)?;

                    if !is_truthy(&left_value) {
                        return Ok(left_value);
                    }

                    Ok(self.evaluate(right)?)
                }
                _ => unreachable!(),
            },
        }
    }

    pub fn evaluate_unary(
        &mut self,
        operator: &Token<'a>,
        right: &Expression<'a>,
    ) -> InterpreterResult<'a, Literal> {
        let value = self.evaluate(&right)?;

        match operator.kind {
            TokenType::Bang => Ok(Literal::Boolean(!is_truthy(&value))),
            TokenType::Minus => match value {
                Literal::Integer(i) => Ok(Literal::Integer(-i)),
                Literal::Float(f) => Ok(Literal::Float(-f)),
                _ => Self::error(operator.clone(), "Operand must be a number"),
            },
            _ => unreachable!(),
        }
    }

    fn evaluate_binary(
        &mut self,
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
            ) => Self::error(operator.clone(), "Operands must be numbers"),
            (TokenType::EqualEqual, left, right) => Ok(Literal::Boolean(left.eq(&right))),
            (TokenType::BangEqual, left, right) => Ok(Literal::Boolean(!left.eq(&right))),
            _ => unreachable!(),
        }
    }
}
