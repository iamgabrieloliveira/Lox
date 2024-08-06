use crate::ast::Literal;
use std::collections::HashMap;

type Value = Option<Literal>;

#[derive(Clone)]
pub struct Environment<'a> {
    values: HashMap<&'a str, Value>,
    pub enclosing: Option<Box<Environment<'a>>>,
}

impl<'a> Environment<'a> {
    pub fn new(enclosing: Option<Box<Environment<'a>>>) -> Self {
        Self {
            enclosing,
            values: HashMap::new(),
        }
    }

    pub fn empty() -> Self {
        Self::new(None)
    }

    pub fn with_enclosing(enclosing: Environment<'a>) -> Self {
        Self::new(Some(Box::new(enclosing)))
    }

    pub fn define(&mut self, name: &'a str, value: Option<Literal>) -> Option<Value> {
        self.values.insert(name, value)
    }

    pub fn get(&self, name: &'a str) -> Option<&Value> {
        self.values.get(&name)
    }
}
