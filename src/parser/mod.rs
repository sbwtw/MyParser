
pub mod recursive_descent;
mod syntax_node;

pub trait Parser {
    fn run(&mut self) -> bool;
}