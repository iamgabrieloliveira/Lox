use crate::types::expression::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct Grouping<'a> {
    pub value: Box<Expression<'a>>,
}
