mod big_integer;
mod boolean;
mod integer;
mod null;
mod return_value;

pub use big_integer::IRBigInteger;
pub use boolean::{IRBoolean, FALSE, TRUE};
pub use integer::IRInteger;
pub use null::{IRNull, NULL};
pub use return_value::IRReturnValue;

use std::fmt;

pub enum IRObjectKind {
    Integer,
    Boolean,
    Null,
}

pub enum IR {
    Integer(IRInteger),
    BigInteger(IRBigInteger),
    Boolean(IRBoolean),
    Null(IRNull),
    ReturnValue(IRReturnValue),
}

impl fmt::Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IR::Integer(ir) => ir.fmt(f),
            IR::BigInteger(ir) => ir.fmt(f),
            IR::Boolean(ir) => ir.fmt(f),
            IR::Null(ir) => ir.fmt(f),
            IR::ReturnValue(ir) => ir.fmt(f),
        }
    }
}

impl PartialEq for IR {
    fn eq(&self, b: &Self) -> bool {
        match (self, b) {
            (IR::Integer(a), IR::Integer(b)) => a.value == b.value,
            (IR::Boolean(a), IR::Boolean(b)) => a.value == b.value,
            (IR::BigInteger(a), IR::BigInteger(b)) => a.value == b.value,
            (IR::Null(_), IR::Null(_)) => true,
            _ => false,
        }
    }
}
