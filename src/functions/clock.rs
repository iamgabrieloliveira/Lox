use crate::ast::Callable;

#[derive(Clone, Debug, PartialEq, Copy)]
pub struct Clock;

impl<'a> Callable<'a> for Clock {
    fn arity(&self) -> usize {
        return 0;
    }

    fn call(
        &self,
        env: Environment<'a>,
        _args: Vec<environment::Value>,
    ) -> crate::ast::CallableReturn<'a> {
        let time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);

        let val = environment::Value::Literal(Literal::Integer(time as i64));

        return (env, val);
    }
}

impl std::fmt::Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}
