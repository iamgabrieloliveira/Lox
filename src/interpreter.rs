use crate::{
    ast::{Expression, Literal, Statement},
    environment::Environment,
    lexer::{Token, TokenType},
};

pub type InterpreterResult<'a> = std::result::Result<Environment<'a>, InterpreterError<'a>>;

pub fn run(statements: Vec<Statement>) {
    let mut env = Environment::head();

    for statement in statements {
        env = execute(statement, env).unwrap();
    }
}

fn execute<'a>(
    statement: Statement<'a>,
    env: Environment<'a>,
) -> Result<Environment<'a>, InterpreterError<'a>> {
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

fn evaluate_unary<'a>(
    operator: Token<'a>,
    right: Box<Expression<'a>>,
    env: Environment<'a>,
) -> Result<(Literal, Environment<'a>), InterpreterError<'a>> {
    let (right, env) = evaluate(*right, env)?;

    let value = match operator.kind {
        TokenType::Minus => match right {
            Literal::Integer(n) => Literal::Integer(-n),
            Literal::Float(n) => Literal::Float(-n),
            _ => panic!("Unary minus can only be applied to numbers"),
        },
        TokenType::Bang => Literal::Boolean(!is_truthy(right)),
        _ => unreachable!(),
    };

    Ok((value, env))
}

fn evaluate_binary<'a>(
    _left: Box<Expression>,
    _operator: &Token,
    _right: Box<Expression>,
) -> Result<Literal, InterpreterError<'a>> {
    Ok(Literal::Integer(1))
}

fn evaluate_var<'a>() {
    todo!()
}

fn evaluate<'a>(
    expression: Expression<'a>,
    env: Environment<'a>,
) -> Result<(Literal, Environment<'a>), InterpreterError<'a>> {
    match expression {
        Expression::Literal(l) => Ok((l.clone(), env)),
        Expression::Grouping(literal) => evaluate(*literal, env),
        Expression::Unary { operator, right } => evaluate_unary(operator, right, env),
        Expression::Binary {
            left,
            operator,
            right,
        } => evaluate_binary(left, &operator, right).map(|_| (Literal::Nil, env)),
        Expression::Variable(token) => {
            let value = env.get_deep(token.lexeme).unwrap().clone();
            Ok((value.unwrap(), env))
        }
        Expression::Assign(token, value) => {
            let (value, mut env) = evaluate(*value, env)?;

            env.define_deep(token.lexeme, Some(value.clone()));

            Ok((value, env))
        }
        Expression::Logical {
            left,
            operator,
            right,
        } => {
            todo!()
        }
    }
}

fn execute_expression<'a>(
    expression: Expression<'a>,
    env: Environment<'a>,
) -> InterpreterResult<'a> {
    evaluate(expression, env).map(|(_, env)| env)
}

fn execute_print<'a>(expression: Expression<'a>, env: Environment<'a>) -> InterpreterResult<'a> {
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

fn execute_block<'a>(
    statements: Vec<Statement<'a>>,
    env: Environment<'a>,
) -> InterpreterResult<'a> {
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
) -> InterpreterResult<'a> {
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
) -> InterpreterResult<'a> {
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

fn is_truthy(value: Literal) -> bool {
    match value {
        Literal::Boolean(v) => v,
        Literal::Nil => false,
        _ => true,
    }
}

#[derive(Debug, Clone)]
pub struct InterpreterError<'a> {
    pub token: Token<'a>,
    pub message: &'a str,
}
