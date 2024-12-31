use std::fmt;

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
