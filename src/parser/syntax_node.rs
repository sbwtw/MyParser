
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
    StmtBlock,
    AssignStmt,
    IfStmt,
    ElseStmt,
    ReturnStmt,
    BreakStmt,
    WhileLoop,
    FuncDefine,
    FuncDeclare,
    FuncArg,
}

pub type SyntaxTree = Tree<SyntaxType>;