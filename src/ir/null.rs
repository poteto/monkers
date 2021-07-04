use std::fmt;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct IRNull();

impl fmt::Display for IRNull {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "null")
    }
}

pub const NULL: IRNull = IRNull();
