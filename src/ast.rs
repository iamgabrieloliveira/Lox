// program -> declaration* EOF ;
// block -> "{" declaration "}"
// declaration → variable_declaration | statement;
// variable_declaration -> "var" IDENTIFIER ( "=" expression )? ";" ;
// statement -> expr_statement | print_statement | block;
// expr_statement -> expression ";" ;
// print_statment -> "print" expression ";" ;
// expression -> literal | unary | binary | grouping ;
// literal -> NUMBER | STRING | "true" | "false" | "nil" ;
// grouping -> "(" expression ")" ;
// unary -> ( "-" | "!" ) expression ;
// binary -> expression operator expression ;
// operator -> "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" ;
// primary -> "true" | "false" | "nil" | NUMBER | STRING | "(" expression ")" | IDENTIFIER;

use std::fmt;

use crate::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    Expression(Expression<'a>),
    Block(Vec<Statement<'a>>),
    Print(Expression<'a>),
    Var {
        name: Token<'a>,
        expression: Option<Expression<'a>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression<'a> {
    Literal(Literal),
    Unary {
        operator: Token<'a>,
        right: Box<Expression<'a>>,
    },
    Assign(Token<'a>, Box<Expression<'a>>),
    Binary {
        left: Box<Expression<'a>>,
        operator: Token<'a>,
        right: Box<Expression<'a>>,
    },
    Grouping(Box<Expression<'a>>),
    Variable(Token<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::String(s) => write!(f, "{}", s),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Nil => write!(f, "nil"),
        }
    }
}
