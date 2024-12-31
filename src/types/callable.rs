use std::fmt::{Debug, Display};

use crate::environment::Environment;

// Callable is the trait that native functions implement
// and is used to call functions user-defined or native.
pub trait Callable<'a>: Debug + Display {
    fn arity(&self) -> usize;
    fn call(
        &self,
        env: Environment<'a>,
        args: Vec<crate::environment::Value<'a>>,
    ) -> (Environment<'a>, crate::environment::Value<'a>);
}

#[derive(Debug)]
pub struct Function<'a> {
    pub declaration: FunctionStatement<'a>,
}
