use crate::{
    ast::{Expression, Literal},
    lexer::{Token, TokenType},
};

pub type InterpreterResult<'a, T> = std::result::Result<T, InterpreterError<'a>>;

#[derive(Debug, Clone)]
pub struct InterpreterError<'a> {
    pub token: Token<'a>,
    pub message: &'a str,
}

pub struct Interpreter {}

impl Interpreter {
    pub fn evaluate<'a>(&self, expr: Expression<'a>) -> InterpreterResult<'a, Literal> {
        match expr {
            Expression::Literal(literal) => Ok(literal.clone()),
            Expression::Unary { operator, right } => {
                let value = self.evaluate(*right)?;

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
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(*left)?;
                let right = self.evaluate(*right)?;

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
                        Ok(Literal::Integer(left / right))
                    }
                    (TokenType::Star, Literal::Float(left), Literal::Float(right)) => {
                        Ok(Literal::Float(left / right))
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
            _ => todo!(),
        }
    }

    fn is_truthy(&self, literal: &Literal) -> bool {
        match literal {
            Literal::Nil => false,
            Literal::Boolean(b) => *b,
            _ => true,
        }
    }
}
