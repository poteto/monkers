use string_interner::{symbol::SymbolU32, Symbol};

use crate::{
    parser::ParserError,
    token::{IntegerSize, Token},
};
use std::fmt;

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
    Return(ReturnStatement),
    Expression(Expression),
    Block(BlockStatement),
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
            Statement::Return(statement) => statement.fmt(f),
            Statement::Expression(expression) => expression.fmt(f),
            Statement::Block(statement) => statement.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{token} {return_value};",
            token = self.token,
            return_value = self.return_value
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statements {
            statement.fmt(f)?;
        }
        Ok(())
    }
}

// Expressions
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerSize),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanExpression),
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
            Expression::Prefix(expression) => expression.fmt(f),
            Expression::Infix(expression) => expression.fmt(f),
            Expression::Boolean(boolean) => boolean.value.fmt(f),
            Expression::If(expression) => expression.fmt(f),
            Expression::Function(expression) => expression.fmt(f),
            Expression::Call(expression) => expression.fmt(f),
            Expression::String(expression) => expression.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub SymbolU32);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Identifier({:?})", self.0.to_usize())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub right: Box<Expression>,
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({operator}{right})",
            operator = self.token,
            right = self.right
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({left} {operator} {right})",
            left = self.left,
            operator = self.token,
            right = self.right
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BooleanExpression {
    pub token: Token,
    pub value: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: Option<BlockStatement>,
    pub alternative: Option<BlockStatement>,
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
    pub body: BlockStatement,
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
