use crate::ast::Literal;
use std::collections::HashMap;

type Value = Option<Literal>;

pub struct Environment<'a> {
    values: HashMap<&'a str, Value>,
    pub parent: Box<Option<Environment<'a>>>,
}

impl<'a> Environment<'a> {
    pub fn new(parent: Option<Environment<'a>>) -> Self {
        Self {
            parent: Box::new(parent),
            values: HashMap::new(),
        }
    }

    pub fn head() -> Self {
        Self::new(None)
    }

    pub fn block(parent: Environment<'a>) -> Self {
        Self::new(Some(parent))
    }

    pub fn get_parent(self) -> Environment<'a> {
        self.parent.unwrap()
    }

    pub fn define(&mut self, name: &'a str, value: Option<Literal>) -> Option<Value> {
        self.values.insert(name, value)
    }

    pub fn define_deep(&mut self, name: &'a str, value: Option<Literal>) -> Option<Value> {
        match self.define(name, value.clone()) {
            Some(v) => Some(v),
            None => match *self.parent {
                Some(ref mut parent) => parent.define(name, value),
                None => None,
            },
        }
    }

    pub fn get(&self, name: &'a str) -> Option<&Value> {
        self.values.get(&name)
    }

    pub fn get_deep(&self, name: &'a str) -> Option<&Value> {
        let mut variable = self.get(name);

        if variable.is_none() {
            variable = match *self.parent {
                Some(ref parent) => parent.get(name),
                None => None,
            };
        }

        variable
    }
}
