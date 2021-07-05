mod env;
mod error;

use string_interner::StringInterner;

use crate::ast::{
    BlockStatement, Expression, Identifier, IfExpression, InfixExpression, PrefixExpression,
    Program, Statement,
};
pub use crate::eval::env::Env;
use crate::eval::error::EvalError;
use crate::ir::{IRFunction, IRReturnValue, IRString, FALSE, IR, NULL, TRUE};
use crate::token::Token;

use std::{cell::RefCell, mem, rc::Rc};

type EvalResult = Result<Rc<IR>, EvalError>;

pub struct Interpreter {
    interner: Rc<RefCell<StringInterner>>,
    env: Rc<RefCell<Env>>,
}

impl Interpreter {
    pub fn new(env: Rc<RefCell<Env>>, interner: Rc<RefCell<StringInterner>>) -> Self {
        Self { env, interner }
    }

    pub fn eval(&mut self, program: &Program) -> EvalResult {
        let statements = &program.statements;
        if statements.is_empty() {
            Ok(Rc::new(IR::Nothing))
        } else {
            self.eval_program(statements)
        }
    }

    fn eval_program(&mut self, statements: &Vec<Statement>) -> EvalResult {
        let mut result = Rc::new(IR::Nothing);
        for statement in statements {
            let value = self.eval_statement(statement)?;
            match &*value {
                IR::ReturnValue(return_value) => return Ok(Rc::clone(&return_value.value)),
                _ => result = value,
            };
        }
        Ok(result)
    }

    fn eval_statement(&mut self, statement: &Statement) -> EvalResult {
        match statement {
            Statement::Let(statement) => {
                let value = self.eval_expression(&statement.value)?;
                self.env
                    .borrow_mut()
                    .set(&statement.name.0, Rc::clone(&value));
                Ok(value)
            }
            Statement::Return(statement) => {
                let value = self.eval_expression(&statement.return_value)?;
                Ok(Rc::new(IR::ReturnValue(IRReturnValue {
                    value: Rc::clone(&value),
                })))
            }
            Statement::Expression(expression) => self.eval_expression(expression),
            Statement::Block(statement) => self.eval_program(&statement.statements),
        }
    }

    fn eval_block_statement(&mut self, block_statement: &BlockStatement) -> EvalResult {
        let mut result = Rc::new(IR::Nothing);
        for statement in &block_statement.statements {
            let value = self.eval_statement(statement)?;
            match &*value {
                IR::ReturnValue(_) => return Ok(value),
                _ => result = value,
            };
        }
        Ok(result)
    }

    fn eval_expression(&mut self, expression: &Expression) -> EvalResult {
        match expression {
            Expression::Identifier(Identifier(identifier_key)) => {
                if let Some(value) = self.env.borrow_mut().get(&identifier_key) {
                    Ok(value)
                } else {
                    let interner = self.interner.borrow_mut();
                    let identifier = interner
                        .resolve(*identifier_key)
                        .expect("Identifier should have been interned");
                    Err(EvalError::UnknownIdentifier(format!("{}", identifier)))
                }
            }
            Expression::Integer(value) => Ok(Rc::new(IR::Integer(*value))),
            Expression::Boolean(expression) => Ok(self.get_interned_bool(expression.value)),
            Expression::Prefix(expression) => {
                let right = self.eval_expression(&expression.right)?;
                self.eval_prefix_expression(&expression, right)
            }
            Expression::Infix(expression) => {
                let left = self.eval_expression(&expression.left)?;
                let right = self.eval_expression(&expression.right)?;
                self.eval_infix_expression(expression, left, right)
            }
            Expression::If(expression) => self.eval_if_expression(expression),
            Expression::Function(expression) => Ok(Rc::new(IR::Function(IRFunction {
                env: Rc::clone(&self.env),
                body: expression.body.clone(),
                parameters: expression.parameters.clone(),
            }))),
            Expression::Call(expression) => {
                let function = self.eval_expression(&expression.function)?;
                let evaluated_args = &expression
                    .arguments
                    .iter()
                    .map(|arg| self.eval_expression(arg))
                    .collect::<Result<Vec<Rc<IR>>, _>>()?;
                self.eval_call_expression(function, evaluated_args)
            }
            Expression::String(string) => {
                let interner = self.interner.borrow_mut();
                let value = interner.resolve(string.value).unwrap();
                Ok(Rc::new(IR::String(IRString {
                    value: value.to_string(),
                })))
            }
        }
    }

    fn eval_prefix_expression(&self, expression: &PrefixExpression, right: Rc<IR>) -> EvalResult {
        match &expression.token {
            Token::Bang => match *right {
                IR::Boolean(TRUE) => Ok(Rc::new(IR::Boolean(FALSE))),
                IR::Boolean(FALSE) => Ok(Rc::new(IR::Boolean(TRUE))),
                IR::Null(NULL) => Ok(Rc::new(IR::Boolean(TRUE))),
                _ => Ok(Rc::new(IR::Boolean(FALSE))),
            },
            Token::Minus => match &*right {
                IR::Integer(integer) => Ok(Rc::new(IR::Integer(-integer))),
                _ => Err(EvalError::UnknownOperator(format!("-{}", right))),
            },
            token => Err(EvalError::UnknownOperator(format!("{}{}", token, right))),
        }
    }

    fn eval_infix_expression(
        &self,
        expression: &InfixExpression,
        left: Rc<IR>,
        right: Rc<IR>,
    ) -> EvalResult {
        match (&*left, &*right) {
            (IR::Integer(left), IR::Integer(right)) => match &expression.token {
                Token::Plus => Ok(Rc::new(IR::Integer(left + right))),
                Token::Minus => Ok(Rc::new(IR::Integer(left - right))),
                Token::Asterisk => Ok(Rc::new(IR::Integer(left * right))),
                Token::Slash => Ok(Rc::new(IR::Integer(left / right))),
                Token::LessThan => Ok(self.get_interned_bool(left < right)),
                Token::GreaterThan => Ok(self.get_interned_bool(left > right)),
                Token::Equal => Ok(self.get_interned_bool(left == right)),
                Token::NotEqual => Ok(self.get_interned_bool(left != right)),
                token => Err(EvalError::UnknownOperator(format!(
                    "{left} {operator} {right}",
                    left = left,
                    operator = token,
                    right = right
                ))),
            },
            (IR::String(left), IR::String(right)) => match &expression.token {
                Token::Plus => Ok(Rc::new(IR::String(IRString {
                    value: left.value.clone() + &right.value,
                }))),
                token => Err(EvalError::UnknownOperator(format!(
                    "{left} {operator} {right}",
                    left = left,
                    operator = token,
                    right = right
                ))),
            },
            (left, right) if expression.token == Token::Equal => {
                Ok(self.get_interned_bool(left == right))
            }
            (left, right) if expression.token == Token::NotEqual => {
                Ok(self.get_interned_bool(left != right))
            }
            (left, right) if mem::discriminant(left) != mem::discriminant(right) => {
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

    fn eval_if_expression(&mut self, expression: &IfExpression) -> EvalResult {
        let condition = self.eval_expression(&expression.condition)?;
        if self.is_truthy(condition) {
            self.eval_block_statement(
                &expression
                    .consequence
                    .as_ref()
                    .expect("Expected consequence"),
            )
        } else if let Some(alternative) = &expression.alternative {
            self.eval_block_statement(alternative)
        } else {
            Ok(Rc::new(IR::Null(NULL)))
        }
    }

    fn eval_call_expression(&mut self, function: Rc<IR>, arguments: &Vec<Rc<IR>>) -> EvalResult {
        match &*function {
            IR::Function(ir_function) => {
                let mut env = Env::with_outer(Rc::clone(&ir_function.env));
                for (Identifier(identifier_key), evaluated_arg) in
                    ir_function.parameters.iter().zip(arguments.iter())
                {
                    env.set(identifier_key, Rc::clone(&evaluated_arg))
                }
                self.env = Rc::new(RefCell::new(env));
                self.eval_block_statement(&ir_function.body)
            }
            ir => Err(EvalError::TypeError(format!("{} is not a function", ir))),
        }
    }

    fn get_interned_bool(&self, native_value: bool) -> Rc<IR> {
        match native_value {
            true => Rc::new(IR::Boolean(TRUE)),
            false => Rc::new(IR::Boolean(FALSE)),
        }
    }

    fn is_truthy(&self, ir: Rc<IR>) -> bool {
        match *ir {
            IR::Null(_) => false,
            IR::Boolean(FALSE) => false,
            IR::Boolean(TRUE) => true,
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};
    use string_interner::StringInterner;

    use crate::eval::{Env, Interpreter};
    use crate::ir::{IRBoolean, IRString, IR};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use super::EvalResult;

    fn test_eval(input: &str) -> EvalResult {
        let interner = Rc::new(RefCell::new(StringInterner::default()));
        let lexer = Lexer::new(input, Rc::clone(&interner));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Env::new()));
        let mut interpreter = Interpreter::new(env, Rc::clone(&interner));

        for error in &program.errors {
            eprintln!("{}", error);
        }

        interpreter.eval(&program)
    }

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
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Integer(value) => {
                        assert_eq!(&expected, value);
                    }
                    ir_object => {
                        panic!("Didn't expect {}", ir_object);
                    }
                },
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
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Boolean(IRBoolean { value }) => {
                        assert_eq!(&expected, value);
                    }
                    ir_object => {
                        panic!("Didn't expect {}", ir_object);
                    }
                },
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
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Boolean(IRBoolean { value }) => {
                        assert_eq!(&expected, value);
                    }
                    ir_object => {
                        panic!("Didn't expect {}", ir_object);
                    }
                },
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
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Integer(value) => {
                        assert_eq!(&expected.unwrap(), value);
                    }
                    IR::Null(_) => {
                        assert!(expected.is_none());
                    }
                    ir_object => {
                        panic!("Didn't expect {}", ir_object);
                    }
                },
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
                r#"
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }
                "#,
                10,
            ),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Integer(value) => {
                        assert_eq!(&expected, value);
                    }
                    ir_object => {
                        panic!("Didn't expect {:?}", ir_object);
                    }
                },
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
                r#"
                if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }
                "#,
                "Unknown Operator: true + false",
            ),
            ("foobar;", "Unknown Identifier: foobar"),
            ("\"Hello\" - \"World\"", "Unknown Operator: Hello - World"),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    ir_object => {
                        panic!("Didn't expect {}", ir_object);
                    }
                },
                Err(err) => {
                    assert_eq!(expected, err.to_string());
                }
            }
        }
    }

    #[test]
    fn it_evaluates_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Integer(value) => {
                        assert_eq!(&expected, value);
                    }
                    ir_object => {
                        panic!("Didn't expect {}", ir_object);
                    }
                },
                Err(err) => {
                    panic!("{}", err);
                }
            }
        }
    }

    #[test]
    fn it_evaluates_call_expressions() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Integer(value) => {
                        assert_eq!(&expected, value);
                    }
                    ir_object => {
                        panic!("Didn't expect {}", ir_object);
                    }
                },
                Err(err) => {
                    panic!("{}", err);
                }
            }
        }
    }

    #[test]
    fn it_evaluates_closures() {
        let tests = vec![(
            r#"
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(2);
            "#,
            4,
        )];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Integer(value) => {
                        assert_eq!(&expected, value);
                    }
                    ir_object => {
                        panic!("Didn't expect {}", ir_object);
                    }
                },
                Err(err) => {
                    panic!("{}", err);
                }
            }
        }
    }

    #[test]
    fn it_evaluates_string_literals() {
        let tests = vec![("\"Hello World!\"", "Hello World!")];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::String(IRString { value }) => {
                        assert_eq!(expected, value);
                    }
                    ir_object => {
                        panic!("Didn't expect {}", ir_object);
                    }
                },
                Err(err) => {
                    panic!("{}", err);
                }
            }
        }
    }
}
