use std::fmt;

#[allow(dead_code)]
pub enum ValidateArgs {
    Zero,
    Exact(usize),
    GreaterThan(usize),
    GreaterThanEqual(usize),
    LessThan(usize),
    LessThanEqual(usize),
    Unchecked,
}

impl fmt::Display for ValidateArgs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValidateArgs::Zero => write!(f, "exactly 0"),
            ValidateArgs::Exact(n) => write!(f, "exactly {}", n),
            ValidateArgs::GreaterThan(n) => write!(f, "greater than {}", n),
            ValidateArgs::GreaterThanEqual(n) => write!(f, "greater than or equal to {}", n),
            ValidateArgs::LessThan(n) => write!(f, "lesser than {}", n),
            ValidateArgs::LessThanEqual(n) => write!(f, "lesser than or equal to {}", n),
            ValidateArgs::Unchecked => write!(f, "any number"),
        }
    }
}
