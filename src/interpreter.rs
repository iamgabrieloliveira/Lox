use std::rc::Rc;

use crate::environment::{Environment, Value};
use crate::lexer::{Token, TokenType};
use crate::types::expression::Literal;
use crate::types::{expression, statement, Expression};
use crate::{functions, types::Statement};

pub enum StatementEffect<'a> {
    Standard(Environment<'a>),
    Return((Environment<'a>, Option<Expression<'a>>)),
}

type StatementResult<'a> = Result<StatementEffect<'a>, InterpreterError<'a>>;

type EvaluationResult<'a> = Result<(Value<'a>, Environment<'a>), InterpreterError<'a>>;

#[derive(Debug, Clone)]
pub struct InterpreterError<'a> {
    pub token: Token<'a>,
    pub message: &'a str,
}

pub fn run(statements: Vec<Statement>) -> Result<(), InterpreterError> {
    let mut env = Environment::head();

    functions::define_all(&mut env);

    for statement in statements {
        match execute(statement, env)? {
            StatementEffect::Standard(e) => env = e,
            StatementEffect::Return(_) => {
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
        Statement::Var(var) => execute_var(var, env),
        Statement::Block(block) => execute_block(block, env),
        Statement::If(r#if) => execute_if(r#if, env),
        Statement::While(r#while) => execute_while(r#while, env),
        // Statement::Break => todo!(),
        Statement::Function(function) => execute_function(function, env),
        Statement::Return(r#return) => {
            let effect = StatementEffect::Return((env, r#return.value));

            Ok(effect)
        }
    }
}

fn execute_function<'a>(
    function: statement::Function<'a>,
    mut env: Environment<'a>,
) -> StatementResult<'a> {
    let name = function.name.lexeme.clone();

    let callable_function = crate::types::callable::Function {
        declaration: function,
    };

    let callable_function = Value::Callable(Rc::new(callable_function));

    env.define(name, callable_function);

    Ok(StatementEffect::Standard(env))
}

pub fn evaluate<'a>(expression: Expression<'a>, env: Environment<'a>) -> EvaluationResult<'a> {
    match expression {
        Expression::Variable(var) => evaluate_var(var, env),
        Expression::Assign(assign) => evaluate_assign(assign, env),
        Expression::Literal(literal) => Ok((Value::Literal(literal), env)),
        Expression::Unary(unary) => evaluate_unary(unary, env),
        Expression::Grouping(grouping) => evaluate(*grouping.value, env),
        Expression::Binary(binary) => evaluate_binary(binary, env),
        Expression::Logical(logical) => evaluate_logical(logical, env),
        Expression::Call(call) => evaluate_function(call, env),
    }
}

fn evaluate_function<'a>(call: expression::Call<'a>, env: Environment<'a>) -> EvaluationResult<'a> {
    let (callee, mut env) = evaluate(*call.callee, env)?;

    match callee {
        Value::Literal(_) => panic!("You can only call functions"),
        Value::Callable(callable) => {
            let mut args: Vec<Value> = Vec::with_capacity(call.arguments.len());

            for arg in call.arguments {
                let (arg, _env) = evaluate(arg, env)?;

                // todo: it don't seems right :|
                env = _env;

                args.push(arg);
            }

            if args.len() != callable.arity() {
                // let message = format!(
                //    "Expected {} arguments but got {}",
                //    callable.arity(),
                //    args.len()
                // );

                // todo: I need a token for error handling
                // return error(call.paren, "Expected n arguments but got m");
            }

            let (env, r#return) = callable.call(env, args)?;

            Ok((r#return, env))
        }
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Literal(v) => match v {
            &Literal::Boolean(v) => v,
            &Literal::Nil => false,
            _ => true,
        },
        Value::Callable(_) => true,
    }
}

fn error<'a, T>(token: Token<'a>, message: &'a str) -> Result<T, InterpreterError<'a>> {
    Err(InterpreterError { token, message })
}

fn evaluate_unary<'a>(unary: expression::Unary<'a>, env: Environment<'a>) -> EvaluationResult<'a> {
    let (right, env) = evaluate(*unary.right, env)?;

    let literal = match right {
        Value::Literal(value) => match unary.operator.kind {
            TokenType::Bang => {
                let value = Value::Literal(value);

                Ok((Literal::Boolean(!is_truthy(&value)), env))
            }
            TokenType::Minus => match value {
                Literal::Integer(i) => Ok((Literal::Integer(-i), env)),
                Literal::Float(f) => Ok((Literal::Float(-f), env)),
                _ => error(unary.operator, "Operand must be a number"),
            },
            _ => unreachable!(),
        },
        Value::Callable(_) => error(unary.operator, "Cannot apply unary operator to a function"),
    };

    literal.map(|(v, e)| (Value::Literal(v), e))
}

fn evaluate_binary<'a>(
    binary: expression::Binary<'a>,
    env: Environment<'a>,
) -> EvaluationResult<'a> {
    let (left, env) = evaluate(*binary.left, env)?;
    let (right, env) = evaluate(*binary.right, env)?;

    match (left, right) {
        (Value::Literal(l), Value::Literal(r)) => {
            // TODO: best error handling
            let value = match (l, binary.operator.kind, r) {
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
                ) => error(binary.operator.clone(), "Operands must numbers")?,

                (l, TokenType::EqualEqual, r) => Literal::Boolean(l == r),
                (l, TokenType::BangEqual, r) => Literal::Boolean(l != r),
                _ => unreachable!(),
            };

            Ok((Value::Literal(value), env))
        }
        _ => error(binary.operator, "Operands must be literals"),
    }
}

fn evaluate_var<'a>(var: expression::Variable<'a>, env: Environment<'a>) -> EvaluationResult<'a> {
    match env.get_deep(var.value.lexeme) {
        Some(v) => Ok((v.clone(), env)),
        None => error(var.value, "Variable not declared"),
    }
}

fn evaluate_assign<'a>(
    assign: expression::Assign<'a>,
    env: Environment<'a>,
) -> EvaluationResult<'a> {
    let (value, mut env) = evaluate(*assign.value, env)?;

    match env.define(assign.name.lexeme, value) {
        Some(v) => Ok((v, env)),
        None => error(
            assign.name,
            "Attempt to assign a variable that does not exist",
        ),
    }
}

fn evaluate_logical<'a>(
    logical: expression::Logical<'a>,
    env: Environment<'a>,
) -> EvaluationResult<'a> {
    match logical.operator.kind {
        TokenType::Or => {
            let (left, env) = evaluate(*logical.left, env)?;

            if is_truthy(&left) {
                return Ok((left, env));
            }

            let (right, env) = evaluate(*logical.right, env)?;

            if is_truthy(&right) {
                return Ok((right, env));
            }

            return Ok((Value::Literal(Literal::Nil), env));
        }
        TokenType::And => {
            let (left, env) = evaluate(*logical.left, env)?;

            if is_truthy(&left) {
                let (right, env) = evaluate(*logical.right, env)?;

                if is_truthy(&right) {
                    return Ok((right, env));
                }

                return Ok((Value::Literal(Literal::Nil), env));
            }

            return Ok((Value::Literal(Literal::Nil), env));
        }
        _ => unreachable!(),
    }
}

fn execute_expression<'a>(expression: Expression<'a>, env: Environment<'a>) -> StatementResult<'a> {
    evaluate(expression, env).map(|(_, env)| StatementEffect::Standard(env))
}

fn execute_print<'a>(print: statement::Print<'a>, env: Environment<'a>) -> StatementResult<'a> {
    evaluate(print.value, env).map(|(value, env)| {
        match value {
            Value::Literal(v) => println!("{}", v),
            Value::Callable(c) => println!("{}", c),
        };

        StatementEffect::Standard(env)
    })
}

fn execute_var<'a>(var: statement::Var<'a>, mut env: Environment<'a>) -> StatementResult<'a> {
    match var.value {
        None => {
            env.define(var.name.lexeme, Value::Literal(Literal::Nil));

            Ok(StatementEffect::Standard(env))
        }
        Some(expr) => {
            let (value, mut env) = evaluate(expr, env)?;

            env.define(var.name.lexeme, value);

            Ok(StatementEffect::Standard(env))
        }
    }
}

pub fn execute_block<'a>(block: statement::Block<'a>, env: Environment<'a>) -> StatementResult<'a> {
    // TODO:
    // Fix Nested Scopes
    // Here we 'borrow' the environment and create a new one with the block as the parent
    // as soon as the block is done executing,
    // we return the parent environment and the block is dropped
    let mut block_env = Environment::block(env);

    for statement in block.statements {
        let result = execute(statement, block_env)?;

        block_env = match result {
            StatementEffect::Standard(env) => env,
            StatementEffect::Return(_) => return Ok(result),
        };
    }

    Ok(StatementEffect::Standard(block_env.get_parent()))
}

fn ensure_is_standard_effect<'a>(effect: StatementEffect<'a>) -> Environment<'a> {
    return match effect {
        StatementEffect::Standard(env) => env,
        _ => unreachable!(
            "I don't like these unreachable, need to think a better way of return statements"
        ),
    };
}

fn execute_if<'a>(r#if: statement::If<'a>, env: Environment<'a>) -> StatementResult<'a> {
    let (value, mut env) = evaluate(r#if.condition, env)?;

    if is_truthy(&value) {
        let effect = execute(*r#if.then, env)?;
        env = ensure_is_standard_effect(effect);
    } else if let Some(o) = *r#if.otherwise {
        let effect = execute(o, env)?;
        env = ensure_is_standard_effect(effect);
    }

    Ok(StatementEffect::Standard(env))
}

fn execute_while<'a>(
    r#while: statement::While<'a>,
    mut env: Environment<'a>,
) -> StatementResult<'a> {
    loop {
        let (value, env_) = evaluate(*r#while.condition.clone(), env)?;

        env = env_;

        if !is_truthy(&value) {
            break;
        }

        let block = Statement::Block(r#while.body.clone());

        let effect = execute(block, env)?;
        env = ensure_is_standard_effect(effect);
    }

    Ok(StatementEffect::Standard(env))
}
