mod boolean;
mod integer;
mod null;

pub use boolean::{IRBoolean, FALSE, TRUE};
pub use integer::IRInteger;
pub use null::{IRNull, NULL};

use std::fmt;

pub enum IRObjectKind {
    Integer,
    Boolean,
    Null,
    NotImplementedYet,
}

pub enum IR {
    Integer(IRInteger),
    Boolean(IRBoolean),
    Null(IRNull),
    NotImplementedYet,
}

impl fmt::Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IR::Integer(ir) => ir.fmt(f),
            IR::Boolean(ir) => ir.fmt(f),
            IR::Null(ir) => ir.fmt(f),
            IR::NotImplementedYet => write!(f, "Not Implemented Yet"),
        }
    }
}

impl PartialEq for IR {
    fn eq(&self, b: &Self) -> bool {
        match (self, b) {
            (IR::Integer(a), IR::Integer(b)) => a.value == b.value,
            (IR::Boolean(a), IR::Boolean(b)) => a.value == b.value,
            (IR::Null(_), IR::Null(_)) => true,
            (IR::NotImplementedYet, IR::NotImplementedYet) => false,
            _ => false,
        }
    }
}
