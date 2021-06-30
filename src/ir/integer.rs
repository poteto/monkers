use super::{IRTypedObject, IRObjectKind};
use std::fmt;

pub struct IRInteger {
    pub value: usize,
}

impl IRTypedObject for IRInteger {
    const TYPE: IRObjectKind = IRObjectKind::Integer;
}

impl fmt::Display for IRInteger {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}
