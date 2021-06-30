use crate::ast::{Expression, Statement, Node};
use crate::ir::{IRInteger, IR};

type Todo = ();

pub fn eval(node: Node) -> Result<IR, Todo> {
    match node {
        Node::Program(program) => eval_statements(program.statements),
        Node::Statement(_) => Ok(IR::NotImplemented),
        Node::Expression(Expression::Integer(value)) => Ok(IR::Integer(IRInteger { value })),
        Node::Expression(_) => Ok(IR::NotImplemented),
    }
}

fn eval_statements(statements: Vec<Statement>) -> Result<IR, Todo> {
    let mut result = Err(());
    for statement in statements {
        result = Ok(eval(Node::Statement(statement))?);
    }
    result
}
