mod boolean;
mod integer;
mod null;

pub use boolean::{IRBoolean, TRUE, FALSE};
pub use integer::IRInteger;
pub use null::{IRNull, NULL};

use std::fmt;

#[derive(PartialEq)]
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
            IR::NotImplementedYet => write!(f, "Not Implemented"),
        }
    }
}
