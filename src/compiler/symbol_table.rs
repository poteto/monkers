use fnv::FnvHashMap;
use string_interner::symbol::SymbolU32;

#[derive(Clone, Debug, PartialEq)]
enum SymbolScope {
    Global,
}

#[derive(Clone)]
pub struct Symbol {
    name: SymbolU32,
    scope: SymbolScope,
    pub index: usize,
}

#[derive(Clone)]
pub struct SymbolTable {
    num_definitions: usize,
    store: FnvHashMap<SymbolU32, Symbol>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self {
            store: Default::default(),
            num_definitions: Default::default(),
        }
    }
}

impl SymbolTable {
    pub fn define(&mut self, name: SymbolU32) -> Symbol {
        let symbol = Symbol {
            name,
            index: self.num_definitions,
            scope: SymbolScope::Global,
        };
        if self.store.insert(name, symbol.clone()).is_none() {
            self.num_definitions += 1;
        }
        symbol
    }

    pub fn resolve(&self, name: SymbolU32) -> Option<Symbol> {
        self.store.get(&name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use string_interner::StringInterner;

    use crate::compiler::symbol_table::SymbolScope;

    use super::SymbolTable;

    #[test]
    fn it_defines_new_symbols() {
        let interner = Rc::new(RefCell::new(StringInterner::default()));
        let mut global = SymbolTable::default();
        let a = global.define(interner.borrow_mut().get_or_intern("a"));
        let b = global.define(interner.borrow_mut().get_or_intern("b"));

        assert_eq!(a.scope, SymbolScope::Global);
        assert_eq!(a.index, 0);
        assert_eq!(b.scope, SymbolScope::Global);
        assert_eq!(b.index, 1);
    }

    #[test]
    fn it_resolves_global_symbols() {
        let interner = Rc::new(RefCell::new(StringInterner::default()));
        let mut global = SymbolTable::default();
        global.define(interner.borrow_mut().get_or_intern("a"));
        global.define(interner.borrow_mut().get_or_intern("b"));

        let expected = vec![("a", SymbolScope::Global, 0), ("b", SymbolScope::Global, 1)];

        for (name, scope, index) in expected {
            match global.resolve(interner.borrow().get(name).unwrap()) {
                Some(symbol) => {
                    assert_eq!(symbol.scope, scope);
                    assert_eq!(symbol.index, index);
                }
                None => {
                    panic!("Expected symbol {} to have been resolved", name);
                }
            }
        }
    }
}
