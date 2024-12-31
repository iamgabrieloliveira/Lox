use crate::types::expression::Expression;
use crate::types::statement::Block;

#[derive(Debug, Clone, PartialEq)]
pub struct While<'a> {
    pub condition: Box<Expression<'a>>,
    pub body: Block<'a>,
}
