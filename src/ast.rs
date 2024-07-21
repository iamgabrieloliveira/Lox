// expression -> literal | unary | binary | grouping ;
// literal -> NUMBER | STRING | "true" | "false" | "nil" ;
// grouping -> "(" expression ")" ;
// unary -> ( "-" | "!" ) expression ;
// binary -> expression operator expression ;
// operator -> "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" ;

use crate::Token;

#[derive(Clone)]
pub enum Expression<'a> {
    Literal(Literal),
    Unary {
        operator: Token<'a>,
        right: Box<Expression<'a>>,
    },
    Binary {
        left: Box<Expression<'a>>,
        operator: Token<'a>,
        right: Box<Expression<'a>>,
    },
    Grouping(Box<Expression<'a>>),
}

#[derive(Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}
