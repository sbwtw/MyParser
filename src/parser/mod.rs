
pub mod recursive_descent;
mod syntax_node;

use self::syntax_node::SyntaxTree;

pub trait Parser {
    fn run(&mut self) -> bool;
    fn syntax_tree(&self) -> &SyntaxTree;
}