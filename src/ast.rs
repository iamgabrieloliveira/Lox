use crate::{
    environment::Environment,
    interpreter::{evaluate, execute_block},
    Token,
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

impl<'a> std::fmt::Display for Function<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{} fn>", self.declaration.name.lexeme)
    }
}
