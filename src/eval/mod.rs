use crate::ast::{Expression, InfixExpression, Node, PrefixExpression, Statement};
use crate::ir::{IRInteger, FALSE, IR, NULL, TRUE};
use crate::token::Token;

type Todo = ();

pub fn eval(node: &Node) -> Result<IR, Todo> {
    match node {
        Node::Program(program) => eval_statements(&program.statements),
        Node::Statement(_) => Ok(IR::NotImplementedYet),
        Node::Expression(expression) => eval_expression(expression),
    }
}

fn eval_statements(statements: &Vec<Statement>) -> Result<IR, Todo> {
    let mut result = Err(());
    for statement in statements {
        match statement {
            Statement::Let(_) => (),
            Statement::Return(_) => (),
            Statement::Expression(expression) => {
                result = eval(&Node::Expression(expression.clone()));
            }
            Statement::Block(_) => (),
        };
    }
    result
}

fn eval_expression(expression: &Expression) -> Result<IR, Todo> {
    match expression {
        Expression::Integer(value) => Ok(IR::Integer(IRInteger {
            value: value.clone(),
        })),
        Expression::Boolean(expression) => Ok(get_interned_bool(expression.value)),
        Expression::Prefix(expression) => {
            // TODO: Ideally we could just borrow `right`, but because `eval`
            // expects an &AST::Node, we need to wrap `right` here with a
            // Node::Expression(expr), which takes ownership of expr.
            let right = eval(&Node::Expression(*expression.right.clone()))?;
            Ok(eval_prefix_expression(&expression, right))
        }
        Expression::Infix(expression) => {
            // TODO: Same problem as above.
            let left = eval(&Node::Expression(*expression.left.clone()))?;
            let right = eval(&Node::Expression(*expression.right.clone()))?;
            Ok(eval_infix_expression(expression, (left, right)))
        }
        _ => Ok(IR::NotImplementedYet),
    }
}

fn eval_prefix_expression(expression: &PrefixExpression, right: IR) -> IR {
    match expression.token {
        Token::Bang => match right {
            IR::Boolean(TRUE) => IR::Boolean(FALSE),
            IR::Boolean(FALSE) => IR::Boolean(TRUE),
            IR::Null(NULL) => IR::Boolean(TRUE),
            _ => IR::Boolean(FALSE),
        },
        Token::Minus => match right {
            IR::Integer(integer) => IR::Integer(IRInteger {
                value: -integer.value,
            }),
            _ => IR::Null(NULL),
        },
        _ => IR::Null(NULL),
    }
}

fn eval_infix_expression(expression: &InfixExpression, arms: (IR, IR)) -> IR {
    match arms {
        (IR::Integer(left), IR::Integer(right)) => match expression.token {
            Token::Plus => IR::Integer(IRInteger {
                value: left.value + right.value,
            }),
            Token::Minus => IR::Integer(IRInteger {
                value: left.value - right.value,
            }),
            Token::Asterisk => IR::Integer(IRInteger {
                value: left.value * right.value,
            }),
            Token::Slash => IR::Integer(IRInteger {
                value: left.value / right.value,
            }),
            Token::LessThan => get_interned_bool(left.value < right.value),
            Token::GreaterThan => get_interned_bool(left.value > right.value),
            Token::Equal => get_interned_bool(left.value == right.value),
            Token::NotEqual => get_interned_bool(left.value != right.value),
            _ => IR::NotImplementedYet,
        },
        (left, right) if expression.token == Token::Equal => get_interned_bool(left == right),
        (left, right) if expression.token == Token::NotEqual => get_interned_bool(left != right),
        _ => IR::Null(NULL),
    }
}

fn get_interned_bool(native_value: bool) -> IR {
    match native_value {
        true => IR::Boolean(TRUE),
        false => IR::Boolean(FALSE),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Node;
    use crate::eval::eval;
    use crate::ir::{IRBoolean, IRInteger, IR};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn it_evaluates_integer_literals() {
        let tests = vec![
            ("5;", 5),
            ("10;", 10),
            ("-5;", -5),
            ("-10;", -10),
            ("5 + 5 + 5 + 5 - 10;", 10),
            ("2 * 2 * 2 * 2 * 2;", 32),
            ("-50 + 100 + -50;", 0),
            ("5 * 2 + 10;", 20),
            ("5 + 2 * 10;", 25),
            ("20 + 2 * -10;", 0),
            ("50 / 2 * 2 + 10;", 60),
            ("2 * (5 + 10);", 30),
            ("3 * 3 * 3 + 10;", 37),
            ("3 * (3 * 3) + 10;", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10;", 50),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            for error in &program.errors {
                eprintln!("{}", error);
            }

            let ir = eval(&Node::Program(program));
            if let Ok(IR::Integer(IRInteger { value })) = ir {
                assert_eq!(expected, value);
            } else {
                panic!("Expected to evalute integers, got {}", ir.unwrap())
            }
        }
    }

    #[test]
    fn it_evaluates_boolean_literals() {
        let tests = vec![
            ("true;", true),
            ("false;", false),
            ("1 < 2;", true),
            ("1 > 2;", false),
            ("1 < 1;", false),
            ("1 > 1;", false),
            ("1 == 1;", true),
            ("1 != 1;", false),
            ("1 == 2;", false),
            ("1 != 2;", true),
            ("true == true;", true),
            ("false == false;", true),
            ("true == false;", false),
            ("true != false;", true),
            ("false != true;", true),
            ("(1 < 2) == true;", true),
            ("(1 < 2) == false;", false),
            ("(1 > 2) == true;", false),
            ("(1 > 2) == false;", true),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            for error in &program.errors {
                eprintln!("{}", error);
            }

            let ir = eval(&Node::Program(program));
            if let Ok(IR::Boolean(IRBoolean { value })) = ir {
                assert_eq!(expected, value);
            } else {
                panic!("Expected to evalute integers, got {}", ir.unwrap())
            }
        }
    }

    #[test]
    fn it_evaluates_bang_operator_expressions() {
        let tests = vec![
            ("!true;", false),
            ("!false;", true),
            ("!5;", false),
            ("!!true;", true),
            ("!!false;", false),
            ("!!5;", true),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            for error in &program.errors {
                eprintln!("{}", error);
            }

            let ir = eval(&Node::Program(program));
            if let Ok(IR::Boolean(IRBoolean { value })) = ir {
                assert_eq!(expected, value);
            } else {
                panic!("Expected to evalute integers, got {}", ir.unwrap())
            }
        }
    }
}
