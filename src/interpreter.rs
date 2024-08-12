use crate::{
    ast::{Expression, Literal, Statement},
    environment::Environment,
    lexer::{Token, TokenType},
};

type EvaluationResult<'a> = Result<(Literal, Environment<'a>), InterpreterError<'a>>;
type StatementResult<'a> = Result<Environment<'a>, InterpreterError<'a>>;

#[derive(Debug, Clone)]
pub struct InterpreterError<'a> {
    pub token: Token<'a>,
    pub message: &'a str,
}

pub fn run(statements: Vec<Statement>) -> Result<(), InterpreterError> {
    let mut env = Environment::head();

    for statement in statements {
        match execute(statement, env) {
            Ok(e) => env = e,
            Err(e) => {
                eprintln!("Error: {:?}", e);
                return Err(e);
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
    }
}

fn evaluate<'a>(expression: Expression<'a>, env: Environment<'a>) -> EvaluationResult<'a> {
    match expression {
        Expression::Variable(token) => evaluate_var(token),
        Expression::Assign(token, value) => evaluate_assign(token, *value),
        Expression::Literal(v) => Ok((v.clone(), env)),
        Expression::Unary { operator, right } => evaluate_unary(operator, right, env),
        Expression::Grouping(literal) => evaluate(*literal, env),
        Expression::Binary {
            left,
            operator,
            right,
        } => evaluate_binary(left, &operator, right, env),
        Expression::Logical {
            left,
            operator,
            right,
        } => evaluate_logical(left, operator, right, env),
    }
}

fn is_truthy(value: Literal) -> bool {
    match value {
        Literal::Boolean(v) => v,
        Literal::Nil => false,
        _ => true,
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

    match operator.kind {
        TokenType::Bang => Ok((Literal::Boolean(!is_truthy(right)), env)),
        TokenType::Minus => match right {
            Literal::Integer(i) => Ok((Literal::Integer(-i), env)),
            Literal::Float(f) => Ok((Literal::Float(-f), env)),
            _ => error(operator, "Operand must be a number"),
        },
        _ => unreachable!(),
    }
}

fn evaluate_binary<'a>(
    _left: Box<Expression>,
    _operator: &Token,
    _right: Box<Expression>,
    _env: Environment<'a>,
) -> EvaluationResult<'a> {
    todo!()
}

fn evaluate_var<'a>(token: Token<'a>) -> EvaluationResult<'a> {
    todo!()
}

fn evaluate_assign<'a>(token: Token<'a>, value: Expression<'a>) -> EvaluationResult<'a> {
    todo!()
}

fn evaluate_logical<'a>(
    left: Box<Expression<'a>>,
    operator: Token<'a>,
    right: Box<Expression<'a>>,
    _env: Environment<'a>,
) -> EvaluationResult<'a> {
    todo!()
}

fn execute_expression<'a>(expression: Expression<'a>, env: Environment<'a>) -> StatementResult<'a> {
    evaluate(expression, env).map(|(_, env)| env)
}

fn execute_print<'a>(expression: Expression<'a>, env: Environment<'a>) -> StatementResult<'a> {
    let (value, env) = evaluate(expression, env)?;

    println!("{}", value);

    Ok(env)
}

fn execute_var<'a>(
    name: Token<'a>,
    expression: Option<Expression<'a>>,
    mut env: Environment<'a>,
) -> Result<Environment<'a>, InterpreterError<'a>> {
    match expression {
        None => {
            env.define(name.lexeme, None);

            Ok(env)
        }
        Some(expr) => {
            let (value, mut env) = evaluate(expr, env)?;

            env.define(name.lexeme, Some(value));

            Ok(env)
        }
    }
}

fn execute_block<'a>(statements: Vec<Statement<'a>>, env: Environment<'a>) -> StatementResult<'a> {
    // Here we 'borrow' the environment and create a new one with the block as the parent
    // as soon as the block is done executing,
    // we return the parent environment and the block is dropped
    let mut block_env = Environment::block(env);

    for statement in statements {
        block_env = execute(statement, block_env)?;
    }

    Ok(block_env.get_parent())
}

fn execute_if<'a>(
    condition: Expression<'a>,
    then: Box<Statement<'a>>,
    otherwise: Box<Option<Statement<'a>>>,
    env: Environment<'a>,
) -> StatementResult<'a> {
    let (value, mut env) = evaluate(condition, env)?;

    if is_truthy(value) {
        env = execute(*then, env)?;
    } else if let Some(e) = *otherwise {
        env = execute(e, env)?;
    }

    Ok(env)
}

fn execute_while<'a>(
    condition: Expression<'a>,
    body: Box<Statement<'a>>,
    mut env: Environment<'a>,
) -> StatementResult<'a> {
    loop {
        let (val, env_) = evaluate(condition.clone(), env)?;

        env = env_;

        if !is_truthy(val) {
            break;
        }

        env = execute(*body.clone(), env)?;
    }

    Ok(env)
}
