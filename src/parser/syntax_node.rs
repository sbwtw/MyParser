
use token::Token;

use id_tree::Tree;

#[derive(Debug)]
pub enum SyntaxType {
    Terminal(Token),
    SyntaxTree,
    Struct,
    Variable,
    Expr,
}

pub type SyntaxTree = Tree<SyntaxType>;
