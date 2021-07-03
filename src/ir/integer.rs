use crate::token::IntegerSize;

use std::fmt;

#[derive(Clone)]
pub struct IRInteger {
    pub value: IntegerSize,
}

impl fmt::Display for IRInteger {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}
