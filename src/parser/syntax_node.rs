
use token::Token;

use id_tree::Tree;

use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum SyntaxType {
    Terminal(Rc<Token>),
    SyntaxTree,
    StructDefine,
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

impl SyntaxType {
    pub fn token(&self) -> Option<Rc<Token>> {
        match *self {
            SyntaxType::Terminal(ref t) => Some(t.clone()),
            _ => None,
        }
    }
}