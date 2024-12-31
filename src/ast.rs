use crate::{
    environment::Environment,
    interpreter::{evaluate, execute_block},
};

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
    fn eq(&self, _other: &Self) -> bool {
        todo!()
    }
}

impl<'a> std::fmt::Display for Function<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{} fn>", self.declaration.name.lexeme)
    }
}
