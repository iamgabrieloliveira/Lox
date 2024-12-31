use crate::{lexer::Token, types::expression::Expression};

#[derive(Debug, Clone, PartialEq)]
pub struct Assign<'a> {
    pub name: Token<'a>,
    pub value: Box<Expression<'a>>,
}
