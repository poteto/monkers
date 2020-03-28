use crate::{
    parser::ParserError,
    token::{IdentifierType, IntegerSize, Token},
};
use std::fmt;

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Program(program) => program.fmt(f),
            Node::Statement(statement) => statement.fmt(f),
            Node::Expression(expression) => expression.fmt(f),
        }
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
#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(statement) => statement.fmt(f),
            Statement::Return(statement) => statement.fmt(f),
            Statement::Expression(expression) => expression.fmt(f),
        }
    }
}

impl Statement {
    pub fn token(&self) -> Option<&Token> {
        match self {
            Statement::Let(statement) => Some(&statement.token),
            Statement::Return(statement) => Some(&statement.token),
            Statement::Expression(_) => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{token} {name} = TODO;",
            token = self.token,
            name = self.name
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{token} TODO;", token = self.token)
    }
}

// Expressions
#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerSize),
    Prefix(PrefixExpression),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Prefix(expression) => {
                write!(f, "({}{})", expression.operator, expression.right)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier(pub IdentifierType);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>, // https://doc.rust-lang.org/book/ch15-01-box.html#enabling-recursive-types-with-boxes
}
