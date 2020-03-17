use std::fmt;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Token {
    Illegal(char),
    EndOfFile,

    Identifier(String),
    Integer(i64),
    String(String),
    Bool(bool),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    Comma,
    Colon,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    Function,
    Let,
    Return,
    If,
    Else,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Illegal(c) => write!(f, "Illegal({})", c),
            Token::EndOfFile => write!(f, "EOF"),

            Token::Identifier(s) => write!(f, "Identifier({})", s),
            Token::Integer(i) => i.fmt(f),
            Token::String(s) => s.fmt(f),
            Token::Bool(b) => write!(f, "Boolean({})", b),

            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),

            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LessThan => write!(f, "<"),
            Token::LessThanEqual => write!(f, "<="),
            Token::GreaterThan => write!(f, ">"),
            Token::GreaterThanEqual => write!(f, ">="),

            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
            Token::Lbrace => write!(f, "{{"),
            Token::Rbrace => write!(f, "}}"),
            Token::Lbracket => write!(f, "["),
            Token::Rbracket => write!(f, "]"),

            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Return => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
        }
    }
}
