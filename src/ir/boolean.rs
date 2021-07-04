use std::fmt;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct IRBoolean {
    pub value: bool,
}

impl fmt::Display for IRBoolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

pub const TRUE: IRBoolean = IRBoolean { value: true };
pub const FALSE: IRBoolean = IRBoolean { value: false };
