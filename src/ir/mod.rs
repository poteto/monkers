mod function;
mod return_value;
mod string;

pub use function::IRFunction;
pub use return_value::IRReturnValue;
pub use string::IRString;

use std::fmt;

use crate::token::IntegerSize;

#[derive(Clone, Debug)]
pub enum IR {
    Nothing,
    Integer(IntegerSize),
    Boolean(bool),
    Null,
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
            IR::Null => write!(f, "null"),
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
            (IR::Boolean(a), IR::Boolean(b)) => a == b,
            (IR::Null, IR::Null) => true,
            _ => false,
        }
    }
}
