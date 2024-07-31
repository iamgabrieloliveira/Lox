use crate::ast::Literal;
use std::collections::HashMap;

pub struct Environment<'a> {
    values: HashMap<&'a str, Option<Literal>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &'a str, value: Option<Literal>) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &'a str) -> Option<&Option<Literal>> {
        self.values.get(&name)
    }
}
