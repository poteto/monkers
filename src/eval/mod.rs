use crate::ast::{Expression, IfExpression, InfixExpression, Node, PrefixExpression, Statement};
use crate::ir::{IRInteger, IRReturnValue, FALSE, IR, NULL, TRUE};
use crate::token::Token;

use std::fmt;

#[derive(Debug)]
pub enum EvalError {
    NotImplementedYet(String),
    InvalidStatement,
    InvalidExpression,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::NotImplementedYet(s) => write!(f, "Not Implemented Yet: {}", s),
            EvalError::InvalidStatement => write!(f, "Statement is invalid"),
            EvalError::InvalidExpression => write!(f, "Expression is invalid"),
        }
    }
}

pub fn eval(node: &Node) -> Result<IR, EvalError> {
    match node {
        Node::Program(program) => eval_statements(&program.statements),
        Node::Statement(statement) => eval_statement(statement),
        Node::Expression(expression) => eval_expression(expression),
    }
}

fn eval_statements(statements: &Vec<Statement>) -> Result<IR, EvalError> {
    let mut result = Err(EvalError::InvalidStatement);
    for statement in statements {
        result = eval_statement(statement);
        if let Ok(IR::ReturnValue(return_value)) = result {
            return Ok(*return_value.value);
        }
    }
    result
}

fn eval_statement(statement: &Statement) -> Result<IR, EvalError> {
    match statement {
        Statement::Let(_) => Err(EvalError::NotImplementedYet("let".to_string())),
        Statement::Return(statement) => {
            if let Ok(value) = eval(&Node::Expression(statement.return_value.clone())) {
                Ok(IR::ReturnValue(IRReturnValue {
                    value: Box::new(value),
                }))
            } else {
                Err(EvalError::InvalidStatement)
            }
        }
        Statement::Expression(expression) => eval(&Node::Expression(expression.clone())),
        Statement::Block(statement) => eval_statements(&statement.statements),
    }
}

fn eval_expression(expression: &Expression) -> Result<IR, EvalError> {
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
            eval_infix_expression(expression, (left, right))
        }
        Expression::If(expression) => eval_if_expression(expression),
        expression => Err(EvalError::NotImplementedYet(expression.to_string())),
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

fn eval_infix_expression(expression: &InfixExpression, arms: (IR, IR)) -> Result<IR, EvalError> {
    match arms {
        (IR::Integer(left), IR::Integer(right)) => match &expression.token {
            Token::Plus => Ok(IR::Integer(IRInteger {
                value: left.value + right.value,
            })),
            Token::Minus => Ok(IR::Integer(IRInteger {
                value: left.value - right.value,
            })),
            Token::Asterisk => Ok(IR::Integer(IRInteger {
                value: left.value * right.value,
            })),
            Token::Slash => Ok(IR::Integer(IRInteger {
                value: left.value / right.value,
            })),
            Token::LessThan => Ok(get_interned_bool(left.value < right.value)),
            Token::GreaterThan => Ok(get_interned_bool(left.value > right.value)),
            Token::Equal => Ok(get_interned_bool(left.value == right.value)),
            Token::NotEqual => Ok(get_interned_bool(left.value != right.value)),
            token => Err(EvalError::NotImplementedYet(token.to_string())),
        },
        (left, right) if expression.token == Token::Equal => Ok(get_interned_bool(left == right)),
        (left, right) if expression.token == Token::NotEqual => {
            Ok(get_interned_bool(left != right))
        }
        _ => Ok(IR::Null(NULL)),
    }
}

fn eval_if_expression(expression: &IfExpression) -> Result<IR, EvalError> {
    if let Some(condition) = &expression.condition {
        let condition = eval(&Node::Expression(*condition.clone()))?;
        if is_truthy(condition) {
            if let Some(consequence) = &expression.consequence {
                // TODO: As above, fix this clone.
                eval(&Node::Statement(Statement::Block(consequence.clone())))
            } else {
                Err(EvalError::InvalidExpression)
            }
        } else if let Some(alternative) = &expression.alternative {
            // TODO: As above, fix this clone.
            eval(&Node::Statement(Statement::Block(alternative.clone())))
        } else {
            Ok(IR::Null(NULL))
        }
    } else {
        Err(EvalError::InvalidExpression)
    }
}

fn get_interned_bool(native_value: bool) -> IR {
    match native_value {
        true => IR::Boolean(TRUE),
        false => IR::Boolean(FALSE),
    }
}

fn is_truthy(ir: IR) -> bool {
    match ir {
        IR::Null(_) => false,
        IR::Boolean(FALSE) => false,
        IR::Boolean(TRUE) => true,
        _ => true,
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
            match ir {
                Ok(IR::Integer(IRInteger { value })) => {
                    assert_eq!(expected, value);
                }
                Ok(ir_object) => {
                    panic!("Didn't expect {}", ir_object);
                }
                Err(err) => {
                    panic!("{}", err);
                }
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
            match ir {
                Ok(IR::Boolean(IRBoolean { value })) => {
                    assert_eq!(expected, value);
                }
                Ok(ir_object) => {
                    panic!("Didn't expect {}", ir_object);
                }
                Err(err) => {
                    panic!("{}", err);
                }
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
            match ir {
                Ok(IR::Boolean(IRBoolean { value })) => {
                    assert_eq!(expected, value);
                }
                Ok(ir_object) => {
                    panic!("Didn't expect {}", ir_object);
                }
                Err(err) => {
                    panic!("{}", err);
                }
            }
        }
    }

    #[test]
    fn it_evaluates_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10 };", Some(10)),
            ("if (false) { 10 };", None),
            ("if (1) { 10 };", Some(10)),
            ("if (1 < 2) { 10 };", Some(10)),
            ("if (1 > 2) { 10 };", None),
            ("if (1 > 2) { 10 } else { 20 };", Some(20)),
            ("if (1 < 2) { 10 } else { 20 };", Some(10)),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            for error in &program.errors {
                eprintln!("{}", error);
            }

            let ir = eval(&Node::Program(program));
            match ir {
                Ok(IR::Integer(IRInteger { value })) => {
                    assert_eq!(expected.unwrap(), value);
                }
                Ok(IR::Null(_)) => {
                    assert!(expected.is_none());
                }
                Ok(ir_object) => {
                    panic!("Didn't expect {}", ir_object);
                }
                Err(err) => {
                    panic!("{}", err);
                }
            }
        }
    }

    #[test]
    fn it_evaluates_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            for error in &program.errors {
                eprintln!("{}", error);
            }

            let ir = eval(&Node::Program(program));
            match ir {
                Ok(IR::Integer(IRInteger { value })) => {
                    assert_eq!(expected, value);
                }
                Ok(ir_object) => {
                    panic!("Didn't expect {}", ir_object);
                }
                Err(err) => {
                    panic!("{}", err);
                }
            }
        }
    }
}
