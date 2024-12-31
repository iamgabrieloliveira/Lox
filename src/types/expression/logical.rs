use crate::{lexer::Token, types::expression::Expression};

#[derive(Clone, Debug, PartialEq)]
pub struct Logical<'a> {
    pub left: Box<Expression<'a>>,
    pub operator: Token<'a>,
    pub right: Box<Expression<'a>>,
}
