use crate::types::expression::Expression;
use crate::types::statement::Statement;

#[derive(Clone, Debug, PartialEq)]
pub struct If<'a> {
    pub condition: Expression<'a>,
    pub then: Box<Statement<'a>>,
    pub otherwise: Box<Option<Statement<'a>>>,
}
