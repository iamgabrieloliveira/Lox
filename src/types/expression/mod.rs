pub mod assign;
pub mod binary;
pub mod call;
pub mod grouping;
pub mod literal;
pub mod logical;
pub mod unary;
pub mod variable;

pub use crate::types::expression::{
    assign::Assign, binary::Binary, call::Call, grouping::Grouping, literal::Literal,
    logical::Logical, unary::Unary, variable::Variable,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression<'a> {
    Literal(Literal),
    Logical(Logical<'a>),
    Unary(Unary<'a>),
    Assign(Assign<'a>),
    Binary(Binary<'a>),
    Grouping(Grouping<'a>),
    Variable(Variable<'a>),
    Call(Call<'a>),
}
