pub mod block;
pub mod function;
pub mod r#if;
pub mod print;
pub mod r#return;
pub mod var;
pub mod r#while;

pub use block::Block;
pub use function::Function;
pub use print::Print;
pub use r#if::If;
pub use r#return::Return;
pub use r#while::While;
pub use var::Var;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    If(If<'a>),
    Return(Return<'a>),
    While(While<'a>),
    Function(Function<'a>),
    Block(Block<'a>),
    Print(Print<'a>),
    Var(Var<'a>),
}
