use std::{rc::Rc, time::SystemTime};

use crate::{
    ast::{Callable, Expression, Function, Literal, Statement},
    environment::{self, Environment},
    lexer::{Token, TokenType},
};

use crate::environment as env;

pub enum StatementResult<'a> {
    Normal(Result<Environment<'a>, InterpreterError<'a>>),
    Returning(Result<(Environment<'a>, Option<Expression<'a>>), InterpreterError<'a>>),
}

type EvaluationResult<'a> = Result<(environment::Value<'a>, Environment<'a>), InterpreterError<'a>>;

#[derive(Debug, Clone)]
pub struct InterpreterError<'a> {
    pub token: Token<'a>,
    pub message: &'a str,
}

pub fn run(statements: Vec<Statement>) -> Result<(), InterpreterError> {
    let mut env = Environment::head();

    let clock_fn = Rc::new(Clock);

    env.define("clock", env::Value::Callable(clock_fn));

    for statement in statements {
        match execute(statement, env) {
            StatementResult::Normal(r) => match r {
                Ok(e) => env = e,
                Err(e) => {
                    eprintln!("Error: {:?}", e);
                    return Err(e);
                }
            },
            StatementResult::Returning(_) => {
                panic!("You can only use 'return' keyword inside a block")
            }
        }
    }

    Ok(())
}

fn execute<'a>(statement: Statement<'a>, env: Environment<'a>) -> StatementResult<'a> {
    match statement {
        Statement::Expression(expr) => execute_expression(expr, env),
        Statement::Print(expr) => execute_print(expr, env),
        Statement::Var { name, expression } => execute_var(name, expression, env),
        Statement::Block(statements) => execute_block(statements, env),
        Statement::If {
            condition,
            then,
            otherwise,
        } => execute_if(condition, then, otherwise, env),
        Statement::While { condition, body } => execute_while(condition, body, env),
        Statement::Break => todo!(),
        Statement::Function(fn_sttm) => execute_function(fn_sttm, env),
        Statement::Return((_keyword, value)) => StatementResult::Returned((Ok(env), value)),
    }
}

fn execute_function<'a>(
    sttm: crate::ast::FunctionStatement<'a>,
    mut env: Environment<'a>,
) -> StatementResult<'a> {
    let function = Function {
        declaration: sttm.clone(),
    };

    let r#fn = crate::environment::Value::Callable(Rc::new(function));

    env.define(sttm.name.lexeme, r#fn);

    StatementResult::Normal(Ok(env))
}

pub fn evaluate<'a>(expression: Expression<'a>, env: Environment<'a>) -> EvaluationResult<'a> {
    match expression {
        Expression::Variable(token) => evaluate_var(token, env),
        Expression::Assign(token, value) => evaluate_assign(token, *value, env),
        Expression::Literal(v) => Ok((environment::Value::Literal(v), env)),
        Expression::Unary { operator, right } => evaluate_unary(operator, right, env),
        Expression::Grouping(literal) => evaluate(*literal, env),
        Expression::Binary {
            left,
            operator,
            right,
        } => evaluate_binary(left, operator, right, env),
        Expression::Logical {
            left,
            operator,
            right,
        } => evaluate_logical(left, operator, right, env),
        Expression::Call {
            callee,
            paren,
            arguments,
        } => evaluate_function(callee, paren, arguments, env),
    }
}

fn evaluate_function<'a>(
    callee: Box<Expression<'a>>,
    paren: Token<'a>,
    arguments: Vec<Expression<'a>>,
    env: Environment<'a>,
) -> EvaluationResult<'a> {
    let (callee, mut env) = evaluate(*callee, env)?;

    match callee {
        environment::Value::Callable(callable) => {
            let mut args: Vec<environment::Value> = Vec::with_capacity(arguments.len());

            for arg in arguments {
                let (arg, _env) = evaluate(arg, env)?;
                env = _env;

                args.push(arg);
            }

            if args.len() != callable.arity() {
                // let message = format!(
                //    "Expected {} arguments but got {}",
                //    callable.arity(),
                //    args.len()
                // );

                return error(paren, "Expected n arguments but got m");
            }

            let (env, r#return) = callable.call(env, args);

            Ok((r#return, env))
        }
        environment::Value::Literal(_) => error(paren, "Can only call functions"),
    }
}

fn is_truthy(value: &environment::Value) -> bool {
    match value {
        environment::Value::Literal(v) => match v {
            &Literal::Boolean(v) => v,
            &Literal::Nil => false,
            _ => true,
        },
        environment::Value::Callable(_) => true,
    }
}

fn error<'a, T>(token: Token<'a>, message: &'a str) -> Result<T, InterpreterError<'a>> {
    Err(InterpreterError { token, message })
}

fn evaluate_unary<'a>(
    operator: Token<'a>,
    right: Box<Expression<'a>>,
    env: Environment<'a>,
) -> EvaluationResult<'a> {
    let (right, env) = evaluate(*right, env)?;

    let literal = match right {
        environment::Value::Literal(value) => match operator.kind {
            TokenType::Bang => {
                let value = environment::Value::Literal(value);

                Ok((Literal::Boolean(!is_truthy(&value)), env))
            }
            TokenType::Minus => match value {
                Literal::Integer(i) => Ok((Literal::Integer(-i), env)),
                Literal::Float(f) => Ok((Literal::Float(-f), env)),
                _ => error(operator, "Operand must be a number"),
            },
            _ => unreachable!(),
        },
        environment::Value::Callable(_value) => {
            error(operator, "Cannot apply unary operator to a function")
        }
    };

    literal.map(|(v, e)| (environment::Value::Literal(v), e))
}

fn evaluate_binary<'a>(
    left: Box<Expression<'a>>,
    operator: Token<'a>,
    right: Box<Expression<'a>>,
    env: Environment<'a>,
) -> EvaluationResult<'a> {
    let (left, env) = evaluate(*left, env)?;
    let (right, env) = evaluate(*right, env)?;

    match (left, right) {
        (environment::Value::Literal(l), environment::Value::Literal(r)) => {
            // TODO: Better error handling
            let value = match (l, operator.kind, r) {
                (Literal::Integer(l), TokenType::Plus, Literal::Integer(r)) => {
                    Literal::Integer(l + r)
                }
                (Literal::Float(l), TokenType::Plus, Literal::Float(r)) => Literal::Float(l + r),

                (Literal::Integer(l), TokenType::Minus, Literal::Integer(r)) => {
                    Literal::Integer(l + r)
                }
                (Literal::Float(l), TokenType::Minus, Literal::Float(r)) => Literal::Float(l + r),

                (Literal::Integer(l), TokenType::Star, Literal::Integer(r)) => {
                    Literal::Integer(l * r)
                }
                (Literal::Float(l), TokenType::Star, Literal::Float(r)) => Literal::Float(l * r),

                (Literal::Integer(l), TokenType::Slash, Literal::Integer(r)) => {
                    Literal::Integer(l / r)
                }
                (Literal::Float(l), TokenType::Slash, Literal::Float(r)) => Literal::Float(l / r),

                (Literal::Integer(l), TokenType::Module, Literal::Integer(r)) => {
                    Literal::Integer(l % r)
                }
                (Literal::Float(l), TokenType::Module, Literal::Float(r)) => Literal::Float(l % r),

                (Literal::String(l), TokenType::Plus, Literal::String(r)) => {
                    Literal::String(format!("{}{}", l, r))
                }

                (Literal::Integer(l), TokenType::Greater, Literal::Integer(r)) => {
                    Literal::Boolean(l > r)
                }
                (Literal::Float(l), TokenType::Greater, Literal::Float(r)) => {
                    Literal::Boolean(l > r)
                }

                (Literal::Integer(l), TokenType::GreaterEqual, Literal::Integer(r)) => {
                    Literal::Boolean(l >= r)
                }
                (Literal::Float(l), TokenType::GreaterEqual, Literal::Float(r)) => {
                    Literal::Boolean(l >= r)
                }

                (Literal::Integer(l), TokenType::Less, Literal::Integer(r)) => {
                    Literal::Boolean(l < r)
                }
                (Literal::Float(l), TokenType::Less, Literal::Float(r)) => Literal::Boolean(l < r),

                (Literal::Integer(l), TokenType::LessEqual, Literal::Integer(r)) => {
                    Literal::Boolean(l <= r)
                }
                (Literal::Float(l), TokenType::LessEqual, Literal::Float(r)) => {
                    Literal::Boolean(l <= r)
                }

                (
                    _,
                    TokenType::Plus
                    | TokenType::Minus
                    | TokenType::Star
                    | TokenType::Slash
                    | TokenType::Greater
                    | TokenType::GreaterEqual
                    | TokenType::Less
                    | TokenType::LessEqual,
                    _,
                ) => error(operator.clone(), "Operands must numbers")?,

                (l, TokenType::EqualEqual, r) => Literal::Boolean(l == r),
                (l, TokenType::BangEqual, r) => Literal::Boolean(l != r),
                _ => unreachable!(),
            };

            Ok((environment::Value::Literal(value), env))
        }
        _ => error(operator, "Operands must be literals"),
    }
}

fn evaluate_var<'a>(token: Token<'a>, env: Environment<'a>) -> EvaluationResult<'a> {
    match env.get_deep(token.lexeme) {
        Some(v) => Ok((v.clone(), env)),
        None => error(token, "Variable not declared"),
    }
}

fn evaluate_assign<'a>(
    token: Token<'a>,
    value: Expression<'a>,
    env: Environment<'a>,
) -> Result<(environment::Value<'a>, Environment<'a>), InterpreterError<'a>> {
    let (value, mut env) = evaluate(value, env)?;

    match env.define(token.lexeme, value) {
        Some(v) => Ok((v, env)),
        None => error(token, "Attempt to assign a variable that does not exist"),
    }
}

fn evaluate_logical<'a>(
    left: Box<Expression<'a>>,
    operator: Token<'a>,
    right: Box<Expression<'a>>,
    env: Environment<'a>,
) -> EvaluationResult<'a> {
    match operator.kind {
        TokenType::Or => {
            let (left, env) = evaluate(*left, env)?;

            if is_truthy(&left) {
                return Ok((left, env));
            }

            let (right, env) = evaluate(*right, env)?;

            if is_truthy(&right) {
                return Ok((right, env));
            }

            return Ok((environment::Value::Literal(Literal::Nil), env));
        }
        TokenType::And => {
            let (left, env) = evaluate(*left, env)?;

            if is_truthy(&left) {
                let (right, env) = evaluate(*right, env)?;

                if is_truthy(&right) {
                    return Ok((right, env));
                }

                return Ok((environment::Value::Literal(Literal::Nil), env));
            }

            return Ok((environment::Value::Literal(Literal::Nil), env));
        }
        _ => unreachable!(),
    }
}

fn execute_expression<'a>(expression: Expression<'a>, env: Environment<'a>) -> StatementResult<'a> {
    let value = evaluate(expression, env).map(|(_, env)| env);
    StatementResult::Normal(value)
}

fn execute_print<'a>(expression: Expression<'a>, env: Environment<'a>) -> StatementResult<'a> {
    match evaluate(expression, env) {
        Ok((value, env)) => {
            match value {
                environment::Value::Literal(v) => println!("{}", v),
                environment::Value::Callable(c) => println!("{}", c),
            }

            StatementResult::Normal(Ok(env))
        }
        Err(e) => StatementResult::Normal(Err(e)),
    }
}

fn execute_var<'a>(
    name: Token<'a>,
    expression: Option<Expression<'a>>,
    mut env: Environment<'a>,
) -> StatementResult<'a> {
    match expression {
        None => {
            env.define(name.lexeme, environment::Value::Literal(Literal::Nil));

            StatementResult::Normal(Ok(env))
        }
        Some(expr) => {
            let (value, mut env) = match evaluate(expr, env) {
                Ok((v, e)) => (v, e),
                Err(e) => return StatementResult::Normal(Err(e)),
            };

            env.define(name.lexeme, value);

            StatementResult::Normal(Ok(env))
        }
    }
}

pub fn execute_block<'a>(
    statements: Vec<Statement<'a>>,
    env: Environment<'a>,
) -> StatementResult<'a> {
    // TODO:
    // Fix Nested Scopes
    // Here we 'borrow' the environment and create a new one with the block as the parent
    // as soon as the block is done executing,
    // we return the parent environment and the block is dropped
    let mut block_env = Environment::block(env);

    for statement in statements {
        block_env = match execute(statement, block_env) {
            StatementResult::Normal(n) => match n {
                Ok(e) => e,
                Err(e) => return StatementResult::Normal(Err(e)),
            },
            StatementResult::Returned(r) => {
                return StatementResult::Returned(r);
            }
        };
    }

    StatementResult::Normal(Ok(block_env.get_parent()))
}

fn execute_if<'a>(
    condition: Expression<'a>,
    then: Box<Statement<'a>>,
    otherwise: Box<Option<Statement<'a>>>,
    env: Environment<'a>,
) -> StatementResult<'a> {
    let (value, mut env) = match evaluate(condition, env) {
        Ok((v, e)) => (v, e),
        Err(e) => return StatementResult::Normal(Err(e)),
    };

    if is_truthy(&value) {
        env = match execute(*then, env) {
            StatementResult::Normal(n) => match n {
                Ok(e) => e,
                Err(e) => return StatementResult::Normal(Err(e)),
            },
            StatementResult::Returned(r) => return StatementResult::Returned(r),
        };
    } else if let Some(e) = *otherwise {
        env = match execute(e, env) {
            StatementResult::Normal(n) => match n {
                Ok(e) => e,
                Err(e) => return StatementResult::Normal(Err(e)),
            },
            StatementResult::Returned(r) => return StatementResult::Returned(r),
        }
    }

    StatementResult::Normal(Ok(env))
}

fn execute_while<'a>(
    condition: Expression<'a>,
    body: Box<Statement<'a>>,
    mut env: Environment<'a>,
) -> StatementResult<'a> {
    loop {
        let (value, env_) = match evaluate(condition.clone(), env) {
            Ok((v, e)) => (v, e),
            Err(e) => return StatementResult::Normal(Err(e)),
        };

        env = env_;

        if !is_truthy(&value) {
            break;
        }

        env = match execute(*body.clone(), env) {
            StatementResult::Normal(n) => match n {
                Ok(e) => e,
                Err(e) => return StatementResult::Normal(Err(e)),
            },
            StatementResult::Returned(r) => return StatementResult::Returned(r),
        }
    }

    StatementResult::Normal(Ok(env))
}
