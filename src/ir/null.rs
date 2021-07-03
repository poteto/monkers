use std::fmt;

#[derive(PartialEq, Eq, Clone)]
pub struct IRNull();

impl fmt::Display for IRNull {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "null")
    }
}

#[allow(dead_code)]
pub const NULL: IRNull = IRNull();
