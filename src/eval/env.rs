use fnv::FnvHashMap;
use string_interner::symbol::SymbolU32;

use crate::ir::IR;

pub struct Env {
    store: FnvHashMap<SymbolU32, IR>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            store: Default::default(),
        }
    }

    pub fn get(&self, key: &SymbolU32) -> Option<&IR> {
        self.store.get(key)
    }

    pub fn set(&mut self, key: &SymbolU32, value: IR) {
        self.store.insert(*key, value);
    }
}
