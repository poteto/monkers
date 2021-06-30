use super::{IRTypedObject, IRObjectKind};
use std::fmt;

pub struct IRNull();

impl IRTypedObject for IRNull {
    const TYPE: IRObjectKind = IRObjectKind::Null;
}

impl fmt::Display for IRNull {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "null")
    }
}
