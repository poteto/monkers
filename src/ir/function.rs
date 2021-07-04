use std::{cell::RefCell, fmt, rc::Rc};

use crate::{
    ast::{BlockStatement, Identifier},
    eval::Env,
};

#[derive(Clone)]
pub struct IRFunction {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Env>>,
}

impl fmt::Display for IRFunction {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        Ok(())
    }
}
