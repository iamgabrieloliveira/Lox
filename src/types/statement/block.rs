use crate::types::statement::Statement;

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
}
