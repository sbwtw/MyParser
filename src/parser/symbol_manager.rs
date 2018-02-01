
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

type SymbolTable<V> = HashMap<String, V>;

pub struct SymbolManager<V> {
    symbols: Vec<SymbolTable<V>>,
    scopes: Vec<String>,
}

/// Using RAII to manage symbol scope
pub struct ScopeGuard<V> {
    symbol_manager: Rc<RefCell<SymbolManager<V>>>,
}

impl<V> ScopeGuard<V> {
    pub fn new(ptr: Rc<RefCell<SymbolManager<V>>>) -> ScopeGuard<V> {
        ptr.borrow_mut().create_scope();
        ScopeGuard { symbol_manager: ptr }
    }
}

impl<V> Drop for ScopeGuard<V> {
    fn drop(&mut self) {
        self.symbol_manager.borrow_mut().destory_scope();
    }
}

impl<V> SymbolManager<V> {
    pub fn new() -> SymbolManager<V> {
        SymbolManager {
            symbols: vec![SymbolTable::new()],
            scopes: vec![ String::new() ],
        }
    }

    pub fn scope_level(&self) -> usize {
        self.symbols.len()
    }

    fn scope_str(&self) -> String {
        self.scopes.join("::")
    }

    pub fn lookup<S: AsRef<str>>(&self, symbol: S) -> Option<&V> {
        let s = symbol.as_ref();
        for table in self.symbols.iter().rev() {
            if table.contains_key(s) {
                return table.get(s);
            }
        }

        None
    }

    #[inline]
    fn create_scope(&mut self) {
        trace!("create_scope");

        self.symbols.push(SymbolTable::new());
    }

    #[inline]
    fn destory_scope(&mut self) {
        trace!("destory_scope");

        let _ = self.symbols.pop();
    }

    pub fn push_symbol<S: AsRef<str>>(&mut self, symbol: S, id: V) -> Result<(), &V> {
        let s = symbol.as_ref();
        let tbl = { self.symbols.last_mut().unwrap() };
        if tbl.contains_key(s) {
            return Err(tbl.get(s).unwrap())
        }

        trace!("symbol added: `{}`", s);

        tbl.insert(s.to_owned(), id);
        Ok(())
    }
}