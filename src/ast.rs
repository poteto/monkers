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
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
    Block(BlockStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(statement) => statement.fmt(f),
            Statement::Return(statement) => statement.fmt(f),
            Statement::Expression(expression) => expression.fmt(f),
            Statement::Block(statement) => statement.fmt(f),
        }
    }
}

impl Statement {
    pub fn token(&self) -> Option<&Token> {
        match self {
            Statement::Let(statement) => Some(&statement.token),
            Statement::Return(statement) => Some(&statement.token),
            Statement::Expression(_) => None,
            Statement::Block(statement) => Some(&statement.token),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{token} TODO;", token = self.token)
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
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => ident.fmt(f),
            Expression::Integer(i) => i.fmt(f),
            Expression::Prefix(expression) => write!(
                f,
                "({operator}{right})",
                operator = expression.operator,
                right = expression.right
            ),
            Expression::Infix(expression) => write!(
                f,
                "({left} {operator} {right})",
                left = expression.left,
                operator = expression.operator,
                right = expression.right
            ),
            Expression::Boolean(boolean) => boolean.value.fmt(f),
            Expression::If(expression) => {
                if let (Some(condition), Some(consequence)) = (
                    expression.condition.as_ref(),
                    expression.consequence.as_ref(),
                ) {
                    write!(
                        f,
                        "{token} {cond} {cons}",
                        token = Token::If,
                        cond = condition,
                        cons = consequence
                    )?;
                    if let Some(alternative) = &expression.alternative {
                        write!(f, " {token} {alt}", token = Token::Else, alt = alternative)?;
                    }
                }
                Ok(())
            }
            Expression::Function(expression) => expression.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub IdentifierType);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BooleanExpression {
    pub token: Token,
    pub value: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Option<Box<Expression>>,
    pub consequence: Option<BlockStatement>,
    pub alternative: Option<BlockStatement>,
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
