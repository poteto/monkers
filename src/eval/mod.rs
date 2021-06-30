use crate::ast::{Expression, Node, Statement};
use crate::ir::{IRInteger, IR};

type Todo = ();

pub fn eval(node: Node) -> Result<IR, Todo> {
    match node {
        Node::Program(program) => eval_statements(program.statements),
        Node::Statement(_) => Ok(IR::NotImplementedYet),
        Node::Expression(Expression::Integer(value)) => Ok(IR::Integer(IRInteger { value })),
        Node::Expression(_) => Ok(IR::NotImplementedYet),
    }
}

fn eval_statements(statements: Vec<Statement>) -> Result<IR, Todo> {
    let mut result = Err(());
    for statement in statements {
        match statement {
            Statement::Let(_) => (),
            Statement::Return(_) => (),
            Statement::Expression(expression) => {
                result = eval(Node::Expression(expression));
            }
            Statement::Block(_) => (),
        };
    }
    result
}

#[cfg(test)]
mod tests {
    use crate::ast::Node;
    use crate::eval::eval;
    use crate::ir::{IRInteger, IR};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn it_evaluates_integer_literals() {
        let tests = vec![("5;", 5), ("10;", 10)];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            for error in &program.errors {
                eprintln!("{}", error);
            }

            let ir = eval(Node::Program(program));
            if let Ok(IR::Integer(IRInteger { value })) = ir {
                assert_eq!(expected, value);
            } else {
                panic!("Expected to evalute integers, got {}", ir.unwrap())
            }
        }
    }
}
