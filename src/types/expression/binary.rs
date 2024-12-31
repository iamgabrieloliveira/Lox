use crate::{lexer::Token, types::expression::Expression};

#[derive(Debug, Clone, PartialEq)]
pub struct Binary<'a> {
    left: Box<Expression<'a>>,
    operator: Token<'a>,
    right: Box<Expression<'a>>,
}
