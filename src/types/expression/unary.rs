use crate::lexer::Token;
use crate::types::expression::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct Unary<'a> {
    pub operator: Token<'a>,
    pub right: Box<Expression<'a>>,
}
