use crate::types::expression::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct Print<'a> {
    pub value: Expression<'a>,
}
