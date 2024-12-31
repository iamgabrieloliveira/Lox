use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Variable<'a> {
    pub value: Token<'a>,
}
