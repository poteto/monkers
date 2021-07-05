use std::{cell::RefCell, rc::Rc};

use fnv::FnvHashMap;
use string_interner::symbol::SymbolU32;

use crate::eval::ir::IR;

#[derive(Debug)]
pub struct Env {
    store: FnvHashMap<SymbolU32, Rc<IR>>,
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
