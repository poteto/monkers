use num::BigInt;

use std::fmt;

pub struct IRBigInteger {
    pub value: BigInt,
}

impl fmt::Display for IRBigInteger {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}
