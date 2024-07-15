//! expression -> literal | unary | binary | grouping ;
//! literal -> NUMBER | STRING | "true" | "false" | "nil" ;
//! grouping -> "(" expression ")" ;
//! unary -> ( "-" | "!" ) expression ;
//! binary -> expression operator expression ;
//! operator -> "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" ;

use crate::Token;

pub trait Expression {
    fn print(&self) -> String;
}

pub struct Binary<'a> {
    pub left: Box<dyn Expression>,
    pub operator: Box<Token<'a>>,
    pub right: Box<dyn Expression>,
}
pub struct Grouping {
    pub expression: Box<dyn Expression>,
}
pub struct Literal<'a> {
    pub value: Box<Token<'a>>,
}
pub struct Unary<'a> {
    pub operator: Box<Token<'a>>,
    pub right: Box<dyn Expression>,
}

impl<'a> Expression for Binary<'a> {
    fn print(&self) -> String {
        format!(
            "{} {} {}",
            self.left.print(),
            self.operator.lexeme,
            self.right.print()
        )
    }
}

impl Expression for Grouping {
    fn print(&self) -> String {
        format!("({})", self.expression.print())
    }
}
impl<'a> Expression for Literal<'a> {
    fn print(&self) -> String {
        self.value.lexeme.to_string()
    }
}
impl<'a> Expression for Unary<'a> {
    fn print(&self) -> String {
        format!("{}{}", self.operator.lexeme, self.right.print())
    }
}

pub fn print_expression<T>(expression: T)
where
    T: Expression,
{
    println!("{}", expression.print());
}
