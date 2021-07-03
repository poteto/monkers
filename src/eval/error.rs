use std::fmt;

#[derive(Debug)]
pub enum EvalError {
    NotImplementedYet(String),
    InvalidStatement(String),
    InvalidExpression(String),
    TypeError(String),
    UnknownOperator(String),
    UnknownIdentifier(String),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::NotImplementedYet(s) => write!(f, "Not Implemented Yet: {}", s),
            EvalError::InvalidStatement(s) => write!(f, "Invalid Statement: {}", s),
            EvalError::InvalidExpression(s) => write!(f, "Invalid Expression: {}", s),
            EvalError::TypeError(s) => write!(f, "Type Error: {}", s),
            EvalError::UnknownOperator(s) => write!(f, "Unknown Operator: {}", s),
            EvalError::UnknownIdentifier(s) => write!(f, "Unknown Identifier: {}", s),
        }
    }
}
