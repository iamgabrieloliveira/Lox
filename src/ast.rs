// program -> declaration* EOF ;
// block -> "{" declaration "}"
// declaration → fn_declaration | variable_declaration | statement;
// fn_declaration  -> "fn" IDENTIFIER "(" parameters? ")" block ;
// parameters  -> IDENTIFIER ( "," IDENTIFIER )* ;
// variable_declaration -> "var" IDENTIFIER ( "=" expression )? ";" ;
// statement -> expr_statement | if_statement | print_statement |  while_statement | block;
// if_statement = "if" "(" condition ")" statement ( "else" statement )?
// expr_statement -> expression ";" ;
// while_statement -> "while" "(" expression ")" statement ;
// print_statment -> "print" expression ";" ;
// expression -> assignment ;
// assignment -> IDENTIFIER "=" assignement | logical_or ;
// logical_or -> logical_and ( "or" logical_and )* ;
// logical_and -> equality ( "and" equality ;
// expression -> literal | unary | binary | grouping ;
// literal -> NUMBER | STRING | "true" | "false" | "nil" ;
// grouping -> "(" expression ")" ;
// unary -> ( "-" | "!" ) expression ;
// binary -> expression operator expression ;
// operator -> "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" ;
// primary -> "true" | "false" | "nil" | NUMBER | STRING | "(" expression ")" | IDENTIFIER;

use std::fmt;

use crate::{environment::Environment, interpreter::execute_block, Token};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    Break,
    Expression(Expression<'a>),
    If {
        condition: Expression<'a>,
        then: Box<Statement<'a>>,
        otherwise: Box<Option<Statement<'a>>>,
    },
    While {
        condition: Expression<'a>,
        body: Box<Statement<'a>>,
    },
    Function(FunctionStatement<'a>),
    Block(Vec<Statement<'a>>),
    Print(Expression<'a>),
    Var {
        name: Token<'a>,
        expression: Option<Expression<'a>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionStatement<'a> {
    pub name: Token<'a>,
    pub params: Vec<Token<'a>>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression<'a> {
    Literal(Literal),
    Logical {
        left: Box<Expression<'a>>,
        operator: Token<'a>,
        right: Box<Expression<'a>>,
    },
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
    Call {
        callee: Box<Expression<'a>>,
        paren: Token<'a>,
        arguments: Vec<Expression<'a>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
}

pub type CallableReturn<'a> = (Environment<'a>, crate::environment::Value<'a>);

pub trait Callable<'a>: std::fmt::Debug + std::fmt::Display {
    fn arity(&self) -> usize;
    fn call(
        &self,
        env: Environment<'a>,
        args: Vec<crate::environment::Value<'a>>,
    ) -> CallableReturn<'a>;
}

impl<'a> PartialEq for dyn Callable<'a> {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

#[derive(Debug)]
pub struct Function<'a> {
    pub declaration: FunctionStatement<'a>,
}

impl<'a> Callable<'a> for Function<'a> {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(
        &self,
        mut env: Environment<'a>,
        args: Vec<crate::environment::Value<'a>>,
    ) -> CallableReturn<'a> {
        for (i, arg) in args.iter().enumerate() {
            match self.declaration.params.get(i) {
                Some(param) => {
                    env.define(param.lexeme, arg.clone());
                }
                None => unreachable!(),
            };
        }

        let env = execute_block(self.declaration.body.clone(), env).unwrap();

        (env, crate::environment::Value::Literal(Literal::Nil))
    }
}

impl<'a> std::fmt::Display for Function<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{} fn>", self.declaration.name.lexeme)
    }
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
