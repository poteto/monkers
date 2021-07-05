use std::{cell::RefCell, fmt, rc::Rc};

use crate::{
    ast::{Identifier, Statement},
    eval::Env,
    token::IntegerSize,
};

#[derive(Clone, Debug)]
pub enum IR {
    Nothing,
    Integer(IntegerSize),
    Boolean(bool),
    Null,
    ReturnValue(Rc<IR>),
    Function(Vec<Identifier>, Rc<Statement>, Rc<RefCell<Env>>),
    String(String),
}

impl fmt::Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IR::Nothing => Ok(()),
            IR::Integer(ir) => ir.fmt(f),
            IR::Boolean(ir) => ir.fmt(f),
            IR::Null => write!(f, "null"),
            IR::ReturnValue(ir) => ir.fmt(f),
            IR::Function(_, _, _) => Ok(()),
            IR::String(ir) => ir.fmt(f),
        }
    }
}

impl PartialEq for IR {
    fn eq(&self, b: &Self) -> bool {
        match (self, b) {
            (IR::Integer(a), IR::Integer(b)) => a == b,
            (IR::Boolean(a), IR::Boolean(b)) => a == b,
            (IR::Null, IR::Null) => true,
            _ => false,
        }
    }
}
