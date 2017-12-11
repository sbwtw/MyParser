
use token::*;
use token::Token::*;
use lexer::Lexer;
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

    pub fn run(&mut self) {
        println!("TypeAnalyzer Result:");
        dump_tree(self.ast, self.ast.root_node_id().unwrap(), 0);
    }
}