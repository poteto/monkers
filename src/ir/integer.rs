use crate::token::IntegerSize;

use std::fmt;

pub struct IRInteger {
    pub value: IntegerSize,
}

impl fmt::Display for IRInteger {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}
