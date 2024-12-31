use crate::lexer::Token;

use super::Block;

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'a> {
    pub name: Token<'a>,
    pub parameters: Vec<Token<'a>>,
    pub body: Block<'a>,
}
