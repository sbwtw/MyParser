
use token::*;
// use token::Token::*;
// use lexer::Lexer;
use parser::*;
use parser::ParseError::*;
use parser::syntax_node::*;
use parser::symbol_manager::*;

use id_tree::*;

use std::rc::Rc;
use std::cell::RefCell;

pub struct SymbolChecker<'t> {
    ast: &'t SyntaxTree,
    symbols: Rc<RefCell<SymbolManager>>,
}

macro_rules! error {
    ($err: ident) => {
        Err(ParseErrInfo{
            err_type: $err,
        })
    };
}

impl<'t> SymbolChecker<'t> {
    pub fn new(ast: &'t SyntaxTree) -> SymbolChecker<'t> {
        SymbolChecker {
            ast: ast,
            symbols: Rc::new(RefCell::new(SymbolManager::new())),
        }
    }

    pub fn check(&self) -> ParserResult {
        let ref root_id = self.ast.root_node_id().unwrap().clone();
        self.check_subtree(root_id)?;

        assert!(self.symbols.borrow().scope_level() == 1);
        Ok(())
    }

    fn push_identifier(&self, id: &NodeId) -> ParserResult {
        match self.ast.get(id).unwrap().data() {
            &SyntaxType::Terminal(ref tok) => {
                match **tok {
                    Token::Identifier(ref i) => {
                        match self.symbols.borrow_mut().push_symbol(i, id) {
                            Err(_) => return error!(MultiDefineError),
                            _ => {},
                        }
                    },
                    _ =>
                        return error!(SemanticError),
                }
            },
            _ => return error!(SemanticError),
        };

        Ok(())
    }

    fn check_subtree(&self, root_id: &NodeId) -> ParserResult {
        for id in self.ast.children_ids(&root_id).unwrap() {
            match self.ast.get(id).unwrap().data() {
                &SyntaxType::StructDefine => self.check_struct(id)?,
                &SyntaxType::FuncDefine |
                &SyntaxType::FuncDeclare => self.check_function(id)?,
                t => println!("unhandled: {:?}", t),
            }
        }

        Ok(())
    }

    fn check_struct(&self, root_id: &NodeId) -> ParserResult {
        let ids: Vec<&NodeId> = self.ast.children_ids(&root_id).unwrap().collect();
        if ids.len() == 2 { self.push_identifier(ids[0])?; }

        let _symbol_guard = ScopeGuard::new(self.symbols.clone());
        for i in 1..ids.len() {
            self.check_variable_define(ids[i])?;
        }

        Ok(())
    }

    fn check_variable_define(&self, root_id: &NodeId) -> ParserResult {
        for id in self.ast.children_ids(root_id).unwrap() {
            match self.ast.get(id).unwrap().data() {
                &SyntaxType::Terminal(ref tok) => {
                    match **tok {
                        Token::Identifier(_) => {
                            self.push_identifier(id)?;
                        },
                        Token::KeyWord(_) => {},
                        _ =>
                            return error!(SemanticError),
                    }
                },
                _ => return error!(SemanticError),
            };
        }

        Ok(())
    }

    fn check_function(&self, id: &NodeId) -> ParserResult {
        let _symbol_guard = ScopeGuard::new(self.symbols.clone());

        Ok(())
    }
}