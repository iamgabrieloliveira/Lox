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
    pub declaration: crate::types::statement::Function<'a>,
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
                None => unreachable!("we should not have more arguments than params, because we check that in the interpreter"),
            };
        }

        match execute_block(self.declaration.body.clone(), env) {
            crate::interpreter::StatementResult::Normal(env) => (
                env.unwrap(/* todo: impl result in call fn */),
                crate::environment::Value::Literal(Literal::Nil),
            ),
            crate::interpreter::StatementResult::Returned((env, value)) => {
                let env = env.unwrap(/* todo: impl result in call fn */);

                match value {
                    Some(v) => {
                        let (v, env) = evaluate(v, env).unwrap(/* todo */);

                        (env, v)
                    }
                    None => (env, crate::environment::Value::Literal(Literal::Nil)),
                }
            }
        }
    }
}
