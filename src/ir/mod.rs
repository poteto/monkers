mod boolean;
mod integer;
mod null;

pub use boolean::IRBoolean;
pub use integer::IRInteger;
pub use null::IRNull;

use std::fmt;

trait IRTypedObject {
    const TYPE: IRObjectKind;
}

enum IRObjectKind {
    Integer,
    Boolean,
    Null,
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
