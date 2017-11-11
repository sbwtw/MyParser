
pub mod recursive_descent;

pub trait Parser {
    fn run(&mut self) -> bool;
}