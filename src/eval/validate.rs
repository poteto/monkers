use std::fmt;

#[allow(dead_code)]
pub enum ValidateLength {
    Zero,
    Exact(usize),
    GreaterThan(usize),
    GreaterThanEqual(usize),
    LessThan(usize),
    LessThanEqual(usize),
    Unchecked,
}

impl fmt::Display for ValidateLength {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValidateLength::Zero => write!(f, "exactly 0"),
            ValidateLength::Exact(n) => write!(f, "exactly {}", n),
            ValidateLength::GreaterThan(n) => write!(f, "greater than {}", n),
            ValidateLength::GreaterThanEqual(n) => write!(f, "greater than or equal to {}", n),
            ValidateLength::LessThan(n) => write!(f, "lesser than {}", n),
            ValidateLength::LessThanEqual(n) => write!(f, "lesser than or equal to {}", n),
            ValidateLength::Unchecked => write!(f, "any number"),
        }
    }
}
