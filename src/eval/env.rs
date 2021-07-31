use std::{cell::RefCell, rc::Rc};

use fnv::FnvHashMap;
use string_interner::symbol::SymbolU32;

use crate::ir::IR;

#[derive(Debug)]
pub struct Env {
    store: FnvHashMap<SymbolU32, Rc<IR>>,
    outer: Option<Rc<RefCell<Env>>>,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            store: Default::default(),
            outer: Default::default(),
        }
    }
}

impl Env {
    pub fn new(outer: Rc<RefCell<Env>>) -> Self {
        Self {
            store: Default::default(),
            outer: Some(outer),
        }
    }

    pub fn with_outer(outer: Rc<RefCell<Env>>) -> Self {
        Env::new(outer)
    }

    pub fn get(&self, key: &SymbolU32) -> Option<Rc<IR>> {
        match self.store.get(key) {
            Some(value) => Some(Rc::clone(value)),
            None => match &self.outer {
                Some(outer) => outer.borrow_mut().get(key),
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: &SymbolU32, value: Rc<IR>) {
        self.store.insert(*key, value);
    }
}
