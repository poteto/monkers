use std::fmt;

#[derive(Clone, Debug)]
pub struct IRString {
    pub value: String,
}

impl fmt::Display for IRString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}
