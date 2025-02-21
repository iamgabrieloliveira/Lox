use crate::environment::{Environment, Value};
use crate::types::callable::CallableReturn;
use crate::types::expression::Literal;
use crate::types::Callable;
use std::fmt;
use std::time::SystemTime;

#[derive(Clone, Debug, PartialEq)]
pub struct Clock;

impl<'a> Callable<'a> for Clock {
    fn arity(&self) -> usize {
        return 0;
    }

    fn call(&self, env: Environment<'a>, _args: Vec<Value>) -> CallableReturn<'a> {
        let time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);

        let val = Value::Literal(Literal::Integer(time as i64));

        return Ok((env, val));
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native fn>")
    }
}
