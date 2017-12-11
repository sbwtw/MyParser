
use token::Token;

use id_tree::Tree;

use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum SyntaxType {
    Terminal(Rc<Token>),
    SyntaxTree,
    Struct,
    VariableDefine,
    Expr,
    BooleanExpr,
    ExprOpt,
    StmtBlock,
    AssignStmt,
    IfStmt,
    ElseStmt,
    ReturnStmt,
    BreakStmt,
    WhileLoop,
    ForLoop,
    FuncDefine,
    FuncDeclare,
    FuncArg,
}

pub type SyntaxTree = Tree<SyntaxType>;