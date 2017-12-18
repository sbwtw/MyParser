
use token::*;
// use token::Token::*;
// use lexer::Lexer;
use parser::*;
use parser::syntax_node::*;
use parser::symbol_manager::*;

use id_tree::*;

use std::rc::Rc;
use std::cell::RefCell;

pub struct SymbolChecker<'t> {
    ast: &'t SyntaxTree,
    symbols: Rc<RefCell<SymbolManager>>,
}

impl<'t> SymbolChecker<'t> {
    pub fn new(ast: &'t SyntaxTree) -> SymbolChecker<'t> {
        SymbolChecker {
            ast: ast,
            symbols: Rc::new(RefCell::new(SymbolManager::new())),
        }
    }

    pub fn check(&self) -> ParserResult {
        let root_id = self.ast.root_node_id().unwrap().clone();

        for id in self.ast.children_ids(&root_id).unwrap() {
            match self.ast.get(id).unwrap().data() {
                &SyntaxType::StructDefine => self.check_struct(id)?,
                &SyntaxType::FuncDefine |
                &SyntaxType::FuncDeclare => self.check_function(id)?,
                t => panic!("Type not handled: {:?}", t),
            }
        }

        assert!(self.symbols.borrow().scope_level() == 1);
        Ok(())
    }

    fn check_struct(&self, id: &NodeId) -> ParserResult {
        let _symbol_guard = ScopeGuard::new(self.symbols.clone());

        Ok(())
    }

    fn check_function(&self, id: &NodeId) -> ParserResult {
        let _symbol_guard = ScopeGuard::new(self.symbols.clone());

        Ok(())
    }
}