use crate::types::expression::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct Call<'a> {
    pub callee: Box<Expression<'a>>,
    pub arguments: Vec<Expression<'a>>,
}
