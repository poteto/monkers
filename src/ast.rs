use string_interner::{symbol::SymbolU32, Symbol};

use crate::{
    parser::ParserError,
    token::{IntegerSize, Token},
};
use std::{fmt, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub SymbolU32);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Identifier({:?})", self.0.to_usize())
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<ParserError>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.statements
            .iter()
            .map(|statement| statement.fmt(f))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Let(LetStatement),
    Return(Expression),
    Block(Rc<Vec<Statement>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetStatement {
    pub ident: Identifier,
    pub expr: Expression,
}

impl LetStatement {
    pub fn new(ident: Identifier, expr: Expression) -> Self {
        Self { ident, expr }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Expression(expression) => expression.fmt(f),
            Statement::Let(stmt) => write!(
                f,
                "{token} {ident} = {expr};",
                token = Token::Let,
                ident = stmt.ident,
                expr = stmt.expr,
            ),
            Statement::Return(value) => {
                write!(f, "{token} {value};", token = Token::Return, value = value)
            }
            Statement::Block(statements) => {
                statements
                    .iter()
                    .map(|statement| statement.fmt(f))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerSize),
    String(SymbolU32),
    Boolean(bool),

    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Index(
        Box<Expression>, // Left
        Box<Expression>, // Index
    ),

    If(
        Box<Expression>,        // Condition
        Option<Box<Statement>>, // Consequence
        Option<Box<Statement>>, // Alternative
    ),
    Function(
        Rc<Vec<Identifier>>, // Parameters
        Rc<Statement>,       // Body
    ),
    Call(
        Box<Expression>, // Function
        Vec<Expression>, // Arguments
    ),

    Array(Vec<Expression>),
    Hash(Vec<(Expression, Expression)>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrefixExpression {
    pub op: Token,
    pub right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new(op: Token, right: Box<Expression>) -> Self {
        Self { op, right }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InfixExpression {
    pub op: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl InfixExpression {
    pub fn new(op: Token, left: Box<Expression>, right: Box<Expression>) -> Self {
        Self { op, left, right }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(identifier) => identifier.fmt(f),
            Expression::Integer(integer) => integer.fmt(f),
            Expression::String(string_key) => {
                write!(f, "String({:?})", string_key.to_usize())
            }
            Expression::Boolean(value) => value.fmt(f),
            Expression::Prefix(PrefixExpression { op, right }) => {
                write!(f, "({operator}{right})", operator = op, right = right)
            }
            Expression::Infix(InfixExpression { op, left, right }) => write!(
                f,
                "({left} {op} {right})",
                left = left,
                op = op,
                right = right
            ),
            Expression::Index(left, index) => {
                write!(f, "({left}[{index}])", left = left, index = index)
            }
            Expression::If(condition, consequence, alternative) => {
                if let (condition, Some(consequence)) = (condition, consequence) {
                    write!(
                        f,
                        "{token} {cond} {cons}",
                        token = Token::If,
                        cond = condition,
                        cons = consequence
                    )?;
                    if let Some(alternative) = alternative {
                        write!(f, " {token} {alt}", token = Token::Else, alt = alternative)?;
                    }
                }
                Ok(())
            }
            Expression::Function(parameters, body) => write!(
                f,
                "{token}({params}) {{{body}}}",
                token = Token::Function,
                params = parameters
                    .iter()
                    .map(|ident| ident.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                body = body
            ),
            Expression::Call(function, arguments) => write!(
                f,
                "{func}({args})",
                func = function,
                args = arguments
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expression::Array(expressions) => write!(
                f,
                "[{}]",
                expressions
                    .iter()
                    .map(|expression| expression.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expression::Hash(kv_pairs) => write!(
                f,
                "{{{}}}",
                kv_pairs
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}
