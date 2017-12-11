
use token::*;
// use token::Token::*;
// use lexer::Lexer;
use parser::*;
use parser::syntax_node::*;

use id_tree::*;

pub struct TypeAnalyzer<'t> {
    ast: &'t SyntaxTree,
}

impl<'t> TypeAnalyzer<'t> {
    pub fn new(ast: &'t SyntaxTree) -> TypeAnalyzer<'t> {
        TypeAnalyzer {
            ast: ast
        }
    }

    pub fn type_of(&self, node: &NodeId) -> Type {
        match self.ast.get(node).unwrap() {
            _ => Type::NoType,
        }
    }
}