use super::{IRTypedObject, IRObjectKind};
use std::fmt;

pub struct IRBoolean {
    value: bool,
}

impl IRTypedObject for IRBoolean {
    const TYPE: IRObjectKind = IRObjectKind::Boolean;
}

impl fmt::Display for IRBoolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}
