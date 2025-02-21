use crate::{lexer::Token, types::expression::Expression};

#[derive(Debug, Clone, PartialEq)]
pub struct Binary<'a> {
    pub left: Box<Expression<'a>>,
    pub operator: Token<'a>,
    pub right: Box<Expression<'a>>,
}
