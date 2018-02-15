
use token::*;
use token::Token::*;
use parser::syntax_node::*;

use id_tree::*;

// use std::rc::Rc;
use std::collections::HashMap;

pub struct TypeAnalyzer<'t> {
    ast: &'t SyntaxTree,
    cache: HashMap<NodeId, Type>,
}

impl<'t> TypeAnalyzer<'t> {
    pub fn new(ast: &'t SyntaxTree) -> TypeAnalyzer<'t> {
        TypeAnalyzer {
            ast: ast,
            cache: HashMap::new(),
        }
    }

    pub fn type_of(&self, node: &NodeId) -> Type {
        if let Some(t) = self.cache.get(node) {
            return t.clone();
        }

        match self.data(node) {
            &SyntaxType::Terminal(ref ptr) => {
                match **ptr {
                    Identifier(_, _) => self.type_of_ident(node),
                    _ => Type::NoType,
                }
            },
            _ => Type::NoType,
        }
    }

    fn type_of_ident(&self, _id: &NodeId) -> Type {
        Type::NoType
    }

    // #[inline]
    // fn token(&self, node_id: &NodeId) -> Option<Rc<Token>> {
    //     self.data(node_id).token()
    // }

    #[inline]
    fn data(&self, node_id: &NodeId) -> &SyntaxType {
        self.ast.get(node_id).unwrap().data()
    }

    // #[inline]
    // fn children_ids(&self, node_id: &NodeId) -> Vec<&NodeId> {
    //     self.ast.children_ids(&node_id).unwrap().collect()
    // }
}