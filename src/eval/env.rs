use std::{cell::RefCell, rc::Rc};

use fnv::FnvHashMap;
use string_interner::symbol::SymbolU32;

use crate::ir::IR;

pub struct Env {
    store: FnvHashMap<SymbolU32, IR>,
    outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            store: Default::default(),
            outer: Default::default(),
        }
    }

    pub fn with_outer(outer: Rc<RefCell<Env>>) -> Self {
        let mut env = Env::new();
        env.outer = Some(outer);
        env
    }

    pub fn get(&self, key: &SymbolU32) -> Option<IR> {
        match self.store.get(key) {
            Some(value) => Some(value.clone()),
            None => match &self.outer {
                Some(outer) => outer.borrow_mut().get(key),
                None => None
            }
        }
    }

    pub fn set(&mut self, key: &SymbolU32, value: IR) {
        self.store.insert(*key, value);
    }
}
