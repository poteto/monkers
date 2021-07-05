use string_interner::{symbol::SymbolU32, Symbol};

use crate::{
    parser::ParserError,
    token::{IntegerSize, Token},
};
use std::{fmt, rc::Rc};

// Identifier
#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub SymbolU32);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Identifier({:?})", self.0.to_usize())
    }
}

// Program
#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<ParserError>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statements {
            statement.fmt(f)?;
        }
        Ok(())
    }
}

// Statements
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let(Token, Identifier, Expression),
    Return(Token, Expression),
    Expression(Expression),
    Block(Token, Vec<Statement>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(token, name, value) => write!(
                f,
                "{token} {name} = {value};",
                token = token,
                name = name,
                value = value,
            ),
            Statement::Return(token, value) => {
                write!(f, "{token} {value};", token = token, value = value)
            }
            Statement::Expression(expression) => expression.fmt(f),
            Statement::Block(_, statements) => {
                for statement in statements {
                    statement.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

// Expressions
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerSize),
    Prefix(Token, Box<Expression>),
    Infix(
        Token,
        Box<Expression>, // Left
        Box<Expression>, // Right
    ),
    Boolean(Token, bool),
    If(
        Token,
        Box<Expression>,        // Condition
        Option<Box<Statement>>, // Consequence
        Option<Box<Statement>>, // Alternative
    ),
    Function(
        Token,
        Vec<Identifier>, // Parameters
        Rc<Statement>,   // Body
    ),
    Call(
        Token,
        Box<Expression>, // Function
        Vec<Expression>, // Arguments
    ),
    String(StringLiteral),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(identifier) => identifier.fmt(f),
            Expression::Integer(integer) => integer.fmt(f),
            Expression::Prefix(token, right) => {
                write!(f, "({operator}{right})", operator = token, right = right)
            }
            Expression::Infix(operator, left, right) => write!(
                f,
                "({left} {operator} {right})",
                left = left,
                operator = operator,
                right = right
            ),
            Expression::Boolean(_, value) => value.fmt(f),
            Expression::If(token, condition, consequence, alternative) => {
                if let (condition, Some(consequence)) = (condition, consequence) {
                    write!(
                        f,
                        "{token} {cond} {cons}",
                        token = token,
                        cond = condition,
                        cons = consequence
                    )?;
                    if let Some(alternative) = alternative {
                        write!(f, " {token} {alt}", token = Token::Else, alt = alternative)?;
                    }
                }
                Ok(())
            }
            Expression::Function(token, parameters, body) => write!(
                f,
                "{token}({params}) {{{body}}}",
                token = token,
                params = parameters
                    .iter()
                    .map(|ident| ident.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                body = body
            ),
            Expression::Call(_, function, arguments) => write!(
                f,
                "{func}({args})",
                func = function,
                args = arguments
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expression::String(expression) => expression.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StringLiteral {
    pub token: Token,
    pub value: SymbolU32,
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "String({:?})", self.value.to_usize())
    }
}
