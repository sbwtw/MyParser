
use token::Token;

use id_tree::Tree;

#[derive(Debug)]
pub enum SyntaxType {
    Terminal(Token),
    SyntaxTree,
    Struct,
    Variable,
    Expr,
    BooleanExpr,
}

pub type SyntaxTree = Tree<SyntaxType>;
