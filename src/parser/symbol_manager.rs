
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

type SymbolTable<V> = HashMap<String, V>;

pub struct SymbolManager<V, S> {
    symbols: Vec<SymbolTable<V>>,
    scopes: Vec<S>,
}

/// Using RAII to manage symbol scope
pub struct ScopeGuard<V, S> {
    symbol_manager: Rc<RefCell<SymbolManager<V, S>>>,
}

impl<V, S> ScopeGuard<V, S> {
    pub fn new(ptr: Rc<RefCell<SymbolManager<V, S>>>, scope: S) -> ScopeGuard<V, S> {
        ptr.borrow_mut().create_scope(scope);
        ScopeGuard { symbol_manager: ptr }
    }
}

impl<V, S> Drop for ScopeGuard<V, S> {
    fn drop(&mut self) {
        self.symbol_manager.borrow_mut().destory_scope();
    }
}

impl<V, S> SymbolManager<V, S> {
    pub fn new() -> SymbolManager<V, S> {
        SymbolManager {
            symbols: vec![SymbolTable::new()],
            scopes: vec![],
        }
    }

    pub fn scope_level(&self) -> usize {
        self.symbols.len()
    }

    // pub fn current_scope(&self) -> Option<&S> {
    //     self.scopes.last()
    // }

    // pub fn current_scope_mut(&mut self) -> Option<&mut S> {
    //     self.scopes.last_mut()
    // }

    pub fn lookup<T: AsRef<str>>(&self, symbol: T) -> Option<&V> {
        let s = symbol.as_ref();
        for table in self.symbols.iter().rev() {
            if table.contains_key(s) {
                return table.get(s);
            }
        }

        None
    }

    #[inline]
    fn create_scope(&mut self, scope: S) {
        trace!("create_scope");

        self.symbols.push(SymbolTable::new());
        self.scopes.push(scope);
    }

    #[inline]
    fn destory_scope(&mut self) {
        trace!("destory_scope");

        let _ = self.symbols.pop();
    }

    pub fn push_symbol<T: AsRef<str>>(&mut self, symbol: T, id: V) -> Result<(), &V> {
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