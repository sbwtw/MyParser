
use id_tree::NodeId;

use std::collections::HashMap;

type SymbolTable = HashMap<String, NodeId>;

pub struct SymbolManager {
    symbols: Vec<SymbolTable>,
}

impl SymbolManager {
    pub fn new() -> SymbolManager {
        SymbolManager {
            symbols: vec![SymbolTable::new()],
        }
    }

    pub fn scope_level(&self) -> usize {
        self.symbols.len()
    }

    pub fn lookup<S: AsRef<str>>(&self, symbol: S) -> Option<&NodeId> {
        let s = symbol.as_ref();
        for table in self.symbols.iter().rev() {
            if table.contains_key(s) {
                return table.get(s);
            }
        }

        None
    }

    #[inline]
    pub fn create_scope(&mut self) {
        trace!("create_scope");

        self.symbols.push(SymbolTable::new());
    }

    #[inline]
    pub fn destory_scope(&mut self) {
        trace!("destory_scope");

        let _ = self.symbols.pop();
    }

    pub fn push_symbol<S: AsRef<str>>(&mut self, symbol: S, id: &NodeId) -> Result<(), &NodeId> {
        let s = symbol.as_ref();
        let tbl = { self.symbols.last_mut().unwrap() };
        if tbl.contains_key(s) {
            return Err(tbl.get(s).unwrap())
        }

        trace!("symbol added: `{}`", s);

        tbl.insert(s.to_owned(), id.clone());
        Ok(())
    }
}