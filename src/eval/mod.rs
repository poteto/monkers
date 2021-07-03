mod error;

use crate::ast::{
    BlockStatement, Expression, IfExpression, InfixExpression, PrefixExpression, Program, Statement,
};
use crate::eval::error::EvalError;
use crate::ir::{IRInteger, IRReturnValue, FALSE, IR, NULL, TRUE};
use crate::token::Token;

use std::mem;

pub fn eval(program: &Program) -> Result<IR, EvalError> {
    let statements = &program.statements;
    if statements.is_empty() {
        Ok(IR::Nothing)
    } else {
        eval_program(statements)
    }
}

fn eval_program(statements: &Vec<Statement>) -> Result<IR, EvalError> {
    let mut result = Ok(IR::Nothing);
    for statement in statements {
        result = eval_statement(statement);
        match result {
            Ok(IR::ReturnValue(return_value)) => {
                return Ok(*return_value.value);
            }
            Err(_) => {
                return result;
            }
            _ => {}
        };
    }
    result
}

fn eval_statement(statement: &Statement) -> Result<IR, EvalError> {
    match statement {
        Statement::Let(_) => Err(EvalError::NotImplementedYet("let".to_string())),
        Statement::Return(statement) => {
            let value = eval_expression(&statement.return_value)?;
            Ok(IR::ReturnValue(IRReturnValue {
                value: Box::new(value),
            }))
        }
        Statement::Expression(expression) => eval_expression(expression),
        Statement::Block(statement) => eval_program(&statement.statements),
    }
}

fn eval_block_statement(block_statement: &BlockStatement) -> Result<IR, EvalError> {
    let mut result = Ok(IR::Nothing);
    for statement in &block_statement.statements {
        result = eval_statement(statement);
        match result {
            Ok(IR::ReturnValue(_)) | Err(_) => {
                return result;
            }
            _ => {}
        };
    }
    result
}

fn eval_expression(expression: &Expression) -> Result<IR, EvalError> {
    match expression {
        Expression::Integer(value) => Ok(IR::Integer(IRInteger {
            value: value.clone(),
        })),
        Expression::Boolean(expression) => Ok(get_interned_bool(expression.value)),
        Expression::Prefix(expression) => {
            let right = eval_expression(&expression.right)?;
            eval_prefix_expression(&expression, right)
        }
        Expression::Infix(expression) => {
            let left = eval_expression(&expression.left)?;
            let right = eval_expression(&expression.right)?;
            eval_infix_expression(expression, (left, right))
        }
        Expression::If(expression) => eval_if_expression(expression),
        expression => Err(EvalError::NotImplementedYet(expression.to_string())),
    }
}

fn eval_prefix_expression(expression: &PrefixExpression, right: IR) -> Result<IR, EvalError> {
    match &expression.token {
        Token::Bang => match right {
            IR::Boolean(TRUE) => Ok(IR::Boolean(FALSE)),
            IR::Boolean(FALSE) => Ok(IR::Boolean(TRUE)),
            IR::Null(NULL) => Ok(IR::Boolean(TRUE)),
            _ => Ok(IR::Boolean(FALSE)),
        },
        Token::Minus => match right {
            IR::Integer(integer) => Ok(IR::Integer(IRInteger {
                value: -integer.value,
            })),
            _ => Err(EvalError::UnknownOperator(format!("-{}", right))),
        },
        token => Err(EvalError::UnknownOperator(format!("{}{}", token, right))),
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
            token => Err(EvalError::UnknownOperator(format!(
                "{left} {operator} {right}",
                left = left,
                operator = token,
                right = right
            ))),
        },
        (left, right) if expression.token == Token::Equal => Ok(get_interned_bool(left == right)),
        (left, right) if expression.token == Token::NotEqual => {
            Ok(get_interned_bool(left != right))
        }
        (left, right) if mem::discriminant(&left) != mem::discriminant(&right) => {
            Err(EvalError::TypeError(format!(
                "{left} {operator} {right}",
                left = left,
                operator = expression.token,
                right = right
            )))
        }
        (left, right) => Err(EvalError::UnknownOperator(format!(
            "{left} {operator} {right}",
            left = left,
            operator = expression.token,
            right = right
        ))),
    }
}

fn eval_if_expression(expression: &IfExpression) -> Result<IR, EvalError> {
    if let Some(condition) = &expression.condition {
        let condition = eval_expression(&condition)?;
        if is_truthy(condition) {
            if let Some(consequence) = &expression.consequence {
                eval_block_statement(consequence)
            } else {
                Err(EvalError::InvalidExpression(expression.to_string()))
            }
        } else if let Some(alternative) = &expression.alternative {
            eval_block_statement(alternative)
        } else {
            Ok(IR::Null(NULL))
        }
    } else {
        Err(EvalError::InvalidExpression(expression.to_string()))
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
    use std::{rc::Rc, cell::RefCell};
    use string_interner::StringInterner;

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
            let interner = Rc::new(RefCell::new(StringInterner::default()));
            let lexer = Lexer::new(input, interner.clone());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            for error in &program.errors {
                eprintln!("{}", error);
            }

            let ir = eval(&program);
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
            let interner = Rc::new(RefCell::new(StringInterner::default()));
            let lexer = Lexer::new(input, interner.clone());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            for error in &program.errors {
                eprintln!("{}", error);
            }

            let ir = eval(&program);
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
            let interner = Rc::new(RefCell::new(StringInterner::default()));
            let lexer = Lexer::new(input, interner.clone());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            for error in &program.errors {
                eprintln!("{}", error);
            }

            let ir = eval(&program);
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
            let interner = Rc::new(RefCell::new(StringInterner::default()));
            let lexer = Lexer::new(input, interner.clone());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            for error in &program.errors {
                eprintln!("{}", error);
            }

            let ir = eval(&program);
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
            (
                r#"if (10 > 1) {
    if (10 > 1) {
        return 10;
    }
    return 1;
}"#,
                10,
            ),
        ];

        for (input, expected) in tests {
            let interner = Rc::new(RefCell::new(StringInterner::default()));
            let lexer = Lexer::new(input, interner.clone());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            for error in &program.errors {
                eprintln!("{}", error);
            }

            let ir = eval(&program);
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
    fn it_handles_errors() {
        let tests = vec![
            ("5 + true;", "Type Error: 5 + true"),
            ("5 + true; 5;", "Type Error: 5 + true"),
            ("-true;", "Unknown Operator: -true"),
            ("true + false;", "Unknown Operator: true + false"),
            ("5; true + false; 5;", "Unknown Operator: true + false"),
            (
                "if (10 > 1) { true + false; }",
                "Unknown Operator: true + false",
            ),
            (
                r#"if (10 > 1) {
    if (10 > 1) {
        return true + false;
    }
    return 1;
}"#,
                "Unknown Operator: true + false",
            ),
        ];

        for (input, expected) in tests {
            let interner = Rc::new(RefCell::new(StringInterner::default()));
            let lexer = Lexer::new(input, interner.clone());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            for error in &program.errors {
                eprintln!("{}", error);
            }

            let ir = eval(&program);
            match ir {
                Ok(ir_object) => {
                    panic!("Didn't expect {}", ir_object);
                }
                Err(err) => {
                    assert_eq!(expected, err.to_string());
                }
            }
        }
    }
}
