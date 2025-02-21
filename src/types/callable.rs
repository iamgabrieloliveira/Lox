use std::fmt::{self, Debug, Display};

use crate::{
    environment::Environment,
    interpreter::{evaluate, execute_block, InterpreterError},
};

pub type CallableReturn<'a> =
    Result<(Environment<'a>, crate::environment::Value<'a>), InterpreterError<'a>>;

// Callable is the trait that native functions implement
// and is used to call functions user-defined or native.
pub trait Callable<'a>: Debug + Display {
    fn arity(&self) -> usize;
    fn call(
        &self,
        env: Environment<'a>,
        args: Vec<crate::environment::Value<'a>>,
    ) -> CallableReturn<'a>;
}

#[derive(Debug)]
pub struct Function<'a> {
    pub declaration: crate::types::statement::Function<'a>,
}

impl<'a> std::fmt::Display for Function<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{} fn>", self.declaration.name.lexeme)
    }
}

impl<'a> Callable<'a> for Function<'a> {
    fn arity(&self) -> usize {
        self.declaration.parameters.len()
    }

    fn call(
        &self,
        mut env: Environment<'a>,
        args: Vec<crate::environment::Value<'a>>,
    ) -> CallableReturn<'a> {
        for (i, arg) in args.iter().enumerate() {
            match self.declaration.parameters.get(i) {
                Some(param) => {
                    env.define(param.lexeme, arg.clone());
                }
                None => unreachable!("we should not have more arguments than params, because we check that in the interpreter"),
            };
        }

        match execute_block(self.declaration.body.clone(), env)? {
            crate::interpreter::StatementEffect::Standard(env) => {
                let val =
                    crate::environment::Value::Literal(crate::types::expression::Literal::Nil);

                Ok((env, val))
            }
            crate::interpreter::StatementEffect::Return((env, value)) => match value {
                Some(v) => evaluate(v, env).map(|(v, e)| (e, v)),
                None => {
                    let val =
                        crate::environment::Value::Literal(crate::types::expression::Literal::Nil);

                    Ok((env, val))
                }
            },
        }
    }
}
