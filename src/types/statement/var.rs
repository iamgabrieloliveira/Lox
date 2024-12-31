use crate::{lexer::Token, types::expression::Expression};

#[derive(Debug, Clone, PartialEq)]
pub struct Var<'a> {
    pub name: Token<'a>,
    pub value: Option<Expression<'a>>,
}
