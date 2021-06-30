use std::fmt;

pub struct IRInteger {
    pub value: isize,
}

impl fmt::Display for IRInteger {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}
