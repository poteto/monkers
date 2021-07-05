mod boolean;
mod function;
mod null;
mod return_value;
mod string;

pub use boolean::{IRBoolean, FALSE, TRUE};
pub use function::IRFunction;
pub use null::{IRNull, NULL};
pub use return_value::IRReturnValue;
pub use string::IRString;

use std::fmt;

use crate::token::IntegerSize;

pub enum IRObjectKind {
    Integer,
    Boolean,
    Null,
}

#[derive(Clone, Debug)]
pub enum IR {
    Nothing,
    Integer(IntegerSize),
    Boolean(IRBoolean),
    Null(IRNull),
    ReturnValue(IRReturnValue),
    Function(IRFunction),
    String(IRString),
}

impl fmt::Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IR::Nothing => Ok(()),
            IR::Integer(ir) => ir.fmt(f),
            IR::Boolean(ir) => ir.fmt(f),
            IR::Null(ir) => ir.fmt(f),
            IR::ReturnValue(ir) => ir.fmt(f),
            IR::Function(ir) => ir.fmt(f),
            IR::String(ir) => ir.fmt(f),
        }
    }
}

impl PartialEq for IR {
    fn eq(&self, b: &Self) -> bool {
        match (self, b) {
            (IR::Integer(a), IR::Integer(b)) => a == b,
            (IR::Boolean(a), IR::Boolean(b)) => a.value == b.value,
            (IR::Null(_), IR::Null(_)) => true,
            _ => false,
        }
    }
}
