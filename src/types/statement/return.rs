use crate::types::expression::Expression;

#[derive(Clone, Debug, PartialEq)]
pub struct Return<'a> {
    pub value: Option<Expression<'a>>,
}
