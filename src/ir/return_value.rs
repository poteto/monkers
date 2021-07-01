use std::fmt;

use super::IR;

pub struct IRReturnValue {
    pub value: Box<IR>,
}

impl fmt::Display for IRReturnValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}
