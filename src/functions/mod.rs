mod clock;

use std::rc::Rc;

use crate::environment::{Environment, Value};

pub fn define_all(env: &mut Environment) {
    env.define("clock", Value::Callable(Rc::new(clock::Clock)));
}
