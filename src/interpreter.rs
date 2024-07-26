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

pub fn evaluate<'a>(expr: Expression<'a>) -> InterpreterResult<'a, Literal> {
    match expr {
        Expression::Literal(literal) => Ok(literal.clone()),
        Expression::Grouping(literal) => evaluate(*literal),
        Expression::Unary { operator, right } => evaluate_unary(operator, *right),
        Expression::Binary {
            left,
            operator,
            right,
        } => evaluate_binary(*left, operator, *right),
    }
}

fn evaluate_unary<'a>(
    operator: Token<'a>,
    right: Expression<'a>,
) -> InterpreterResult<'a, Literal> {
    let value = evaluate(right)?;

    match operator.kind {
        TokenType::Bang => Ok(Literal::Boolean(!is_truthy(&value))),
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

fn evaluate_binary<'a>(
    left: Expression<'a>,
    operator: Token<'a>,
    right: Expression<'a>,
) -> InterpreterResult<'a, Literal> {
    let left = evaluate(left)?;
    let right = evaluate(right)?;

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

fn is_truthy(literal: &Literal) -> bool {
    match literal {
        Literal::Nil => false,
        Literal::Boolean(b) => *b,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expression;

    #[test]
    fn literal() {
        let expression = Expression::Literal(Literal::Integer(1));

        let result = evaluate(expression).unwrap();

        assert_eq!(result, Literal::Integer(1));
    }

    #[test]
    fn grouping() {
        let literal = Expression::Literal(Literal::Integer(1));
        let expression = Expression::Grouping(Box::new(literal));

        let result = evaluate(expression).unwrap();

        assert_eq!(result, Literal::Integer(1));
    }

    #[test]
    fn binary() {
        let left = Expression::Literal(Literal::Integer(1));
        let operator = Token {
            kind: TokenType::Plus,
            lexeme: "+",
            literal: crate::lexer::Literal::None,
            line: 1,
        };
        let right = Expression::Literal(Literal::Integer(3));

        let expression = Expression::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        };

        let result = evaluate(expression).unwrap();

        assert_eq!(result, Literal::Integer(4));
    }

    #[test]
    fn unary() {
        let operator = Token {
            kind: TokenType::Minus,
            lexeme: "-",
            literal: crate::lexer::Literal::None,
            line: 1,
        };
        let right = Expression::Literal(Literal::Integer(1));

        let expression = Expression::Unary {
            operator,
            right: Box::new(right),
        };

        let result = evaluate(expression).unwrap();

        assert_eq!(result, Literal::Integer(-1));
    }
}
