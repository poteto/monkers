use crate::token::Token;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum ParserError {
    SyntaxError(ParserErrorMessage),
    UnhandledInfixOperator(Token),
    UnhandledPrefixOperator(Token),
    UnhandledToken(Token),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::SyntaxError(error) => error.fmt(f),
            ParserError::UnhandledInfixOperator(token) => {
                write!(f, "Unhandled infix operator: `{}`", token)
            }
            ParserError::UnhandledPrefixOperator(token) => {
                write!(f, "Unhandled prefix operator: `{}`", token)
            }
            ParserError::UnhandledToken(token) => write!(f, "Unhandled token: `{}`", token),
        }
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct ParserErrorMessage {
    pub message: String,
    pub row: usize,
    pub col: usize,
}

impl fmt::Display for ParserErrorMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[Row: {}, Col: {}] {}", self.row, self.col, self.message)
    }
}
