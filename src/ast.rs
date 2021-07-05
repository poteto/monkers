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
    Infix(Token, Box<Expression>, Box<Expression>),
    Boolean(Token, bool),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
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
            Expression::If(expression) => expression.fmt(f),
            Expression::Function(expression) => expression.fmt(f),
            Expression::Call(expression) => expression.fmt(f),
            Expression::String(expression) => expression.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: Option<Box<Statement>>,
    pub alternative: Option<Box<Statement>>,
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let (condition, Some(consequence)) = (&self.condition, &self.consequence) {
            write!(
                f,
                "{token} {cond} {cons}",
                token = Token::If,
                cond = condition,
                cons = consequence
            )?;
            if let Some(alternative) = &self.alternative {
                write!(f, " {token} {alt}", token = Token::Else, alt = alternative)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Rc<Statement>,
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{token}({params}) {{{body}}}",
            token = Token::Function,
            params = &self
                .parameters
                .iter()
                .map(|ident| ident.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            body = self.body
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{func}({args})",
            func = self.function,
            args = self
                .arguments
                .iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
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
