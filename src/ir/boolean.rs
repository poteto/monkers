use super::{IRTypedObject, IRObjectKind};
use std::fmt;

pub struct IRBoolean {
    pub value: bool,
}

impl IRTypedObject for IRBoolean {
    const TYPE: IRObjectKind = IRObjectKind::Boolean;
}

impl fmt::Display for IRBoolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

pub const TRUE: IRBoolean = IRBoolean { value: true };
pub const FALSE: IRBoolean = IRBoolean { value: false };
