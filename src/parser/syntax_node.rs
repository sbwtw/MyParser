
use token::Token;

use id_tree::Tree;

#[derive(Debug, PartialEq)]
pub enum SyntaxType {
    Terminal(Token),
    SyntaxTree,
    Struct,
    VariableDefine,
    Expr,
    BooleanExpr,
    AssignStmt,
    IfStmt,
    ElseStmt,
}

pub type SyntaxTree = Tree<SyntaxType>;