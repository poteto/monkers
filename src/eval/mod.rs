mod env;
mod error;
mod validate;

use fnv::FnvHashMap;
use string_interner::StringInterner;

pub use crate::eval::env::Env;
use crate::{
    ast::{
        CallExpression, Expression, FunctionExpression, Identifier, IfExpression, IndexExpression,
        InfixExpression, LetStatement, PrefixExpression, Program, Statement,
    },
    eval::error::EvalError,
    eval::validate::ValidateLength,
    ir::{BuiltIn, InternedString, IR},
    token::{IntegerSize, Token},
};

use std::{cell::RefCell, mem, rc::Rc};

type EvalResult = Result<Rc<IR>, EvalError>;

const TRUE: IR = IR::Boolean(true);
const FALSE: IR = IR::Boolean(false);
const NULL: IR = IR::Null;

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

    fn eval_program(&mut self, statements: &[Statement]) -> EvalResult {
        let mut result = Rc::new(IR::Nothing);
        for statement in statements {
            let value = self.eval_statement(statement)?;
            match &*value {
                IR::ReturnValue(value) => return Ok(Rc::clone(value)),
                _ => result = value,
            };
        }
        Ok(result)
    }

    fn eval_statement(&mut self, statement: &Statement) -> EvalResult {
        match statement {
            Statement::Let(LetStatement {
                ident: Identifier(identifier_key),
                expr,
            }) => {
                let value = self.eval_expression(expr)?;
                self.env.borrow_mut().set(identifier_key, Rc::clone(&value));
                Ok(value)
            }
            Statement::Return(value) => {
                let value = self.eval_expression(value)?;
                Ok(Rc::new(IR::ReturnValue(Rc::clone(&value))))
            }
            Statement::Expression(expression) => self.eval_expression(expression),
            Statement::Block(_) => self.eval_block_statement(statement),
        }
    }

    fn eval_block_statement(&mut self, block_statement: &Statement) -> EvalResult {
        let mut result = Rc::new(IR::Nothing);
        if let Statement::Block(statements) = block_statement {
            for statement in statements.iter() {
                let value = self.eval_statement(statement)?;
                match &*value {
                    IR::ReturnValue(_) => return Ok(value),
                    _ => result = value,
                };
            }
        }
        Ok(result)
    }

    fn eval_expressions(&mut self, expressions: &[Expression]) -> Result<Vec<Rc<IR>>, EvalError> {
        expressions
            .iter()
            .map(|arg| self.eval_expression(arg))
            .collect::<Result<Vec<Rc<IR>>, _>>()
    }

    fn eval_expression(&mut self, expression: &Expression) -> EvalResult {
        match expression {
            Expression::Identifier(Identifier(identifier_key)) => {
                match self.env.borrow_mut().get(identifier_key) {
                    Some(value) => Ok(value),
                    None => {
                        let interner = self.interner.borrow_mut();
                        let identifier = interner
                            .resolve(*identifier_key)
                            .expect("Identifier should have been interned");
                        match identifier {
                            "len" => Ok(Rc::new(IR::StdLib(BuiltIn::Len))),
                            "last" => Ok(Rc::new(IR::StdLib(BuiltIn::Last))),
                            "head" => Ok(Rc::new(IR::StdLib(BuiltIn::Head))),
                            "tail" => Ok(Rc::new(IR::StdLib(BuiltIn::Tail))),
                            "push" => Ok(Rc::new(IR::StdLib(BuiltIn::Push))),
                            "puts" => Ok(Rc::new(IR::StdLib(BuiltIn::Puts))),
                            _ => Err(EvalError::UnknownIdentifier(identifier.to_string())),
                        }
                    }
                }
            }
            Expression::Integer(value) => Ok(Rc::new(IR::Integer(*value))),
            Expression::Boolean(value) => Ok(self.get_interned_bool(value)),
            Expression::Prefix(PrefixExpression { op, right }) => {
                let right = self.eval_expression(right)?;
                self.eval_prefix_expression(op, right)
            }
            Expression::Infix(InfixExpression { op, left, right }) => {
                let left = self.eval_expression(left)?;
                let right = self.eval_expression(right)?;
                self.eval_infix_expression(op, left, right)
            }
            Expression::Index(IndexExpression { left, index }) => {
                let left = self.eval_expression(left)?;
                let index = self.eval_expression(index)?;
                self.eval_index_expression(left, index)
            }
            Expression::If(IfExpression {
                condition,
                consequent,
                alternate,
            }) => self.eval_if_expression(condition, consequent, alternate),
            Expression::Function(FunctionExpression { params, body }) => Ok(Rc::new(IR::Function(
                Rc::clone(params),
                Rc::clone(body),
                Rc::clone(&self.env),
            ))),
            Expression::Call(CallExpression { func, args }) => {
                // quote(<Expression>)
                if let Expression::Identifier(Identifier(identifier_key)) = **func {
                    let mut interner = self.interner.borrow_mut();
                    let quote_key = interner.get_or_intern(String::from("quote"));
                    if quote_key == identifier_key {
                        self.expect_arguments_length(args, ValidateLength::Exact(1))?;
                        return Ok(Rc::new(IR::Quote(args[0].clone())));
                    }
                }
                let function = self.eval_expression(func)?;
                let evaluated_args = self.eval_expressions(args)?;
                self.eval_call_expression(function, &evaluated_args)
            }
            Expression::String(string_key) => Ok(Rc::new(IR::String(InternedString {
                string_key: *string_key,
                interner: Rc::clone(&self.interner),
            }))),
            Expression::Array(values) => Ok(Rc::new(IR::Array(self.eval_expressions(values)?))),
            Expression::Hash(pairs) => self.eval_hash_literal(pairs),
        }
    }

    fn eval_prefix_expression(&self, op: &Token, right: Rc<IR>) -> EvalResult {
        match op {
            Token::Bang => match *right {
                IR::Boolean(true) => Ok(Rc::new(FALSE)),
                IR::Boolean(false) => Ok(Rc::new(TRUE)),
                IR::Null => Ok(Rc::new(TRUE)),
                _ => Ok(Rc::new(FALSE)),
            },
            Token::Minus => match &*right {
                IR::Integer(integer) => Ok(Rc::new(IR::Integer(-integer))),
                _ => Err(EvalError::UnknownOperator(format!("-{}", right))),
            },
            operator => Err(EvalError::UnknownOperator(format!("{}{}", operator, right))),
        }
    }

    fn eval_infix_expression(&self, op: &Token, left: Rc<IR>, right: Rc<IR>) -> EvalResult {
        match (&*left, &*right) {
            (IR::ReturnValue(left), IR::ReturnValue(right)) => {
                self.eval_infix_expression(op, Rc::clone(left), Rc::clone(right))
            }
            (IR::ReturnValue(return_value), _) => {
                self.eval_infix_expression(op, Rc::clone(return_value), right)
            }
            (_, IR::ReturnValue(return_value)) => {
                self.eval_infix_expression(op, left, Rc::clone(return_value))
            }
            (IR::Integer(left), IR::Integer(right)) => match op {
                Token::Plus => Ok(Rc::new(IR::Integer(left + right))),
                Token::Minus => Ok(Rc::new(IR::Integer(left - right))),
                Token::Asterisk => Ok(Rc::new(IR::Integer(left * right))),
                Token::Slash => Ok(Rc::new(IR::Integer(left / right))),
                Token::LessThan => Ok(self.get_interned_bool(&(left < right))),
                Token::LessThanEqual => Ok(self.get_interned_bool(&(left <= right))),
                Token::GreaterThan => Ok(self.get_interned_bool(&(left > right))),
                Token::GreaterThanEqual => Ok(self.get_interned_bool(&(left >= right))),
                Token::Equal => Ok(self.get_interned_bool(&(left == right))),
                Token::NotEqual => Ok(self.get_interned_bool(&(left != right))),
                token => Err(EvalError::UnknownOperator(format!(
                    "{left} {operator} {right}",
                    left = left,
                    operator = token,
                    right = right
                ))),
            },
            (IR::String(left), IR::String(right)) => match op {
                Token::Plus => {
                    let concat = format!("{}{}", left, right);
                    let mut interner = self.interner.borrow_mut();
                    let string_key = interner.get_or_intern(concat);
                    Ok(Rc::new(IR::String(InternedString {
                        string_key,
                        interner: Rc::clone(&self.interner),
                    })))
                }
                token => Err(EvalError::UnknownOperator(format!(
                    "{left} {operator} {right}",
                    left = left,
                    operator = token,
                    right = right
                ))),
            },
            (left, right) if *op == Token::Equal => Ok(self.get_interned_bool(&(left == right))),
            (left, right) if *op == Token::NotEqual => Ok(self.get_interned_bool(&(left != right))),
            (left, right) if mem::discriminant(left) != mem::discriminant(right) => {
                Err(EvalError::TypeError(format!(
                    "{left} {operator} {right}",
                    left = left,
                    operator = op,
                    right = right
                )))
            }
            (left, right) => Err(EvalError::UnknownOperator(format!(
                "{left} {operator} {right}",
                left = left,
                operator = op,
                right = right
            ))),
        }
    }

    fn eval_index_expression(&mut self, left: Rc<IR>, index: Rc<IR>) -> EvalResult {
        match (&*left, &*index) {
            (IR::Array(values), IR::Integer(index)) => {
                let index = *index as usize;
                match values.get(index) {
                    Some(ir) => Ok(Rc::clone(ir)),
                    None => Ok(Rc::new(NULL)),
                }
            }
            (IR::Hash(map), _) => match map.get(&index) {
                Some(ir) => Ok(Rc::clone(ir)),
                None => Ok(Rc::new(NULL)),
            },
            (left, _) => Err(EvalError::InvalidExpression(format!(
                "Index operator not supported: {}",
                left
            ))),
        }
    }

    fn eval_if_expression(
        &mut self,
        condition: &Expression,
        consequence: &Option<Box<Statement>>,
        alternative: &Option<Box<Statement>>,
    ) -> EvalResult {
        let condition = self.eval_expression(condition)?;
        if self.is_truthy(condition) {
            self.eval_block_statement(consequence.as_ref().expect("Expected consequence"))
        } else if let Some(alternative) = alternative {
            self.eval_block_statement(alternative)
        } else {
            Ok(Rc::new(NULL))
        }
    }

    fn eval_call_expression(&mut self, function: Rc<IR>, arguments: &[Rc<IR>]) -> EvalResult {
        match &*function {
            IR::Function(parameters, body, env) => {
                self.expect_arguments_length(arguments, ValidateLength::Exact(parameters.len()))?;
                let mut inner_env = Env::with_outer(Rc::clone(env));
                for (Identifier(identifier_key), evaluated_arg) in
                    parameters.iter().zip(arguments.iter())
                {
                    inner_env.set(identifier_key, Rc::clone(evaluated_arg))
                }
                // Obtain a reference to the current env to avoid polluting it with function locals.
                // Then, temporarily set the outer env to the function's inner env, so we can
                // evaluate the function's body with the correct locals. We need this because the
                // interpreter is stateful and we don't thread the env through to each eval function.
                let current_env = Rc::clone(&self.env);
                self.env = Rc::new(RefCell::new(inner_env));
                let ret = self.eval_block_statement(body);
                // Reset the outer env to the correct scope.
                self.env = current_env;
                ret
            }
            IR::StdLib(built_in) => self.eval_built_in(built_in, arguments),
            ir => Err(EvalError::TypeError(format!("{} is not a function", ir))),
        }
    }

    fn eval_hash_literal(&mut self, pairs: &[(Expression, Expression)]) -> EvalResult {
        let mut map = FnvHashMap::with_capacity_and_hasher(pairs.len(), Default::default());
        for (k, v) in pairs {
            let k = self.eval_expression(k)?;
            let v = self.eval_expression(v)?;
            map.insert(k, v);
        }
        Ok(Rc::new(IR::Hash(map)))
    }

    fn eval_built_in(&mut self, built_in: &BuiltIn, arguments: &[Rc<IR>]) -> EvalResult {
        match built_in {
            BuiltIn::Len => {
                self.expect_arguments_length(arguments, ValidateLength::Exact(1))?;
                match &*arguments[0] {
                    IR::String(string_key) => Ok(Rc::new(IR::Integer(
                        string_key.to_string().len() as IntegerSize,
                    ))),
                    IR::Array(values) => Ok(Rc::new(IR::Integer(values.len() as IntegerSize))),
                    ir => Err(EvalError::TypeError(format!(
                        "Argument to {} not supported, got {}",
                        BuiltIn::Len,
                        ir
                    ))),
                }
            }
            BuiltIn::Last => {
                self.expect_arguments_length(arguments, ValidateLength::Exact(1))?;
                match &*arguments[0] {
                    IR::Array(values) => match values.last() {
                        Some(last) => Ok(Rc::clone(last)),
                        None => Ok(Rc::new(IR::Null)),
                    },
                    ir => Err(EvalError::TypeError(format!(
                        "Argument to {} not supported, got {}",
                        BuiltIn::Last,
                        ir
                    ))),
                }
            }
            BuiltIn::Head => {
                self.expect_arguments_length(arguments, ValidateLength::Exact(1))?;
                match &*arguments[0] {
                    IR::Array(values) => match values.first() {
                        Some(head) => Ok(Rc::clone(head)),
                        None => Ok(Rc::new(IR::Null)),
                    },
                    ir => Err(EvalError::TypeError(format!(
                        "Argument to {} not supported, got {}",
                        BuiltIn::Head,
                        ir
                    ))),
                }
            }
            BuiltIn::Tail => {
                self.expect_arguments_length(arguments, ValidateLength::Exact(1))?;
                match &*arguments[0] {
                    IR::Array(values) => match values.split_first() {
                        Some((_, tail)) => Ok(Rc::new(IR::Array(tail.to_vec()))),
                        None => Ok(Rc::new(IR::Null)),
                    },
                    ir => Err(EvalError::TypeError(format!(
                        "Argument to {} not supported, got {}",
                        BuiltIn::Tail,
                        ir
                    ))),
                }
            }
            BuiltIn::Push => {
                self.expect_arguments_length(arguments, ValidateLength::GreaterThanEqual(2))?;
                if let Some((head, tail)) = arguments.split_first() {
                    match &*Rc::clone(head) {
                        IR::Array(values) => {
                            let mut values = values.clone();
                            values.append(&mut tail.to_vec());
                            Ok(Rc::new(IR::Array(values)))
                        }
                        ir => Err(EvalError::TypeError(format!(
                            "Argument to {} not supported, got {}",
                            BuiltIn::Push,
                            ir
                        ))),
                    }
                } else {
                    unreachable!()
                }
            }
            BuiltIn::Puts => {
                arguments.iter().for_each(|ir| println!("{}", ir));
                Ok(Rc::new(NULL))
            }
        }
    }

    fn expect_arguments_length<T>(
        &self,
        arguments: &[T],
        expected: ValidateLength,
    ) -> Result<(), EvalError> {
        let is_valid = match expected {
            ValidateLength::Zero => arguments.is_empty(),
            ValidateLength::Exact(expected_length) => arguments.len() == expected_length,
            ValidateLength::GreaterThan(expected_length) => arguments.len() > expected_length,
            ValidateLength::GreaterThanEqual(expected_length) => arguments.len() >= expected_length,
            ValidateLength::LessThan(expected_length) => arguments.len() < expected_length,
            ValidateLength::LessThanEqual(expected_length) => arguments.len() <= expected_length,
        };
        if is_valid {
            Ok(())
        } else {
            Err(EvalError::InvalidExpression(format!(
                "Wrong number of arguments, got {}, expected {}",
                arguments.len(),
                expected
            )))
        }
    }

    fn get_interned_bool(&self, native_value: &bool) -> Rc<IR> {
        match native_value {
            true => Rc::new(TRUE),
            false => Rc::new(FALSE),
        }
    }

    fn is_truthy(&self, ir: Rc<IR>) -> bool {
        match *ir {
            IR::Null => false,
            IR::Boolean(false) => false,
            IR::Boolean(true) => true,
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};
    use string_interner::StringInterner;

    use crate::{
        eval::{Env, Interpreter},
        ir::IR,
        lexer::Lexer,
        parser::Parser,
        token::IntegerSize,
    };

    use super::EvalResult;

    fn test_eval(input: &str) -> EvalResult {
        let interner = Rc::new(RefCell::new(StringInterner::default()));
        let lexer = Lexer::new(input, Rc::clone(&interner));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Env::default()));
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
            ("1 <= 1;", true),
            ("1 >= 1;", true),
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
                    IR::Boolean(value) => {
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
                    IR::Boolean(value) => {
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
                    IR::Null => {
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
                Ok(ir) => panic!("Didn't expect {}", *ir),
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
                    IR::String(value) => {
                        assert_eq!(expected, value.to_string());
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
    fn it_evaluates_built_in_len() {
        let tests = vec![
            ("len(\"\");", Ok(0)),
            ("len(\"four\");", Ok(4)),
            ("len(\"hello world\");", Ok(11)),
            ("len([1, 2, 3]);", Ok(3)),
            (
                "len(1);",
                Err(String::from(
                    "Type Error: Argument to len not supported, got 1",
                )),
            ),
            (
                "len(\"one\", \"two\");",
                Err(String::from(
                    "Invalid Expression: Wrong number of arguments, got 2, expected exactly 1",
                )),
            ),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Integer(value) => {
                        assert_eq!(&expected.unwrap(), value);
                    }
                    ir_object => {
                        panic!("Didn't expect {}", ir_object);
                    }
                },
                Err(err) => {
                    assert_eq!(expected.err(), Some(err.to_string()));
                }
            }
        }
    }

    #[test]
    fn it_evaluates_stdlib_array_functions() {
        let tests = vec![(
            r#"
            let map = fn(arr, f) {
                let iter = fn(arr, acc) {
                    if (len(arr) == 0) {
                        acc
                    } else {
                        iter(tail(arr), push(acc, f(head(arr))));
                    }
                };
                iter(arr, []);
            };
            let a = [1, 2, 3, 4];
            map(a, fn(x) { x * 2 });
            "#,
            vec![2, 4, 6, 8],
        )];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Array(values) => {
                        for (i, value) in values.iter().enumerate() {
                            if let IR::Integer(int) = **value {
                                assert_eq!(int, expected[i] as IntegerSize);
                            }
                        }
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
    fn it_evaluates_array_literals() {
        let tests = vec![("[1, 2 * 2, 3 + 3];", vec![1, 4, 6])];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Array(values) => {
                        for (i, value) in values.iter().enumerate() {
                            if let IR::Integer(int) = **value {
                                assert_eq!(int, expected[i] as IntegerSize);
                            }
                        }
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
    fn it_evaluates_array_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0];", Some(1)),
            ("[1, 2, 3][1];", Some(2)),
            ("[1, 2, 3][2];", Some(3)),
            ("let i = 0; [1][i];", Some(1)),
            ("[1, 2, 3][1 + 1];", Some(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Some(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Some(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                Some(2),
            ),
            ("[1, 2, 3][3]", None),
            ("[1, 2, 3][-1]", None),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Integer(value) => {
                        assert_eq!(&expected.unwrap(), value);
                    }
                    IR::Null => {
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
    fn it_evaluates_hash_literals() {
        let tests = vec![(
            r#"
            let two = "two";
            {
                "one": 10 - 9,
                two: 1 + 1,
                "thr" + "ee": 6 / 2,
                4: 4,
                true: 5,
                false: 6
            }
            "#,
            "{three: 3, 4: 4, false: 6, two: 2, true: 5, one: 1}",
        )];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => assert_eq!(format!("{}", *ir), expected),
                Err(err) => {
                    panic!("{}", err);
                }
            }
        }
    }

    #[test]
    fn it_evaluates_hash_index_expressions() {
        let tests = vec![
            ("{\"foo\": 5}[\"foo\"];", Some(5)),
            ("{\"foo\": 5}[\"bar\"];", None),
            ("let key = \"foo\"; {\"foo\": 5}[key];", Some(5)),
            ("{}[\"foo\"]", None),
            ("{5: 5}[5]", Some(5)),
            ("{true: 5}[true]", Some(5)),
            ("{false: 5}[false]", Some(5)),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Integer(value) => {
                        assert_eq!(&expected.unwrap(), value);
                    }
                    IR::Null => {
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
    fn it_evaluates_quote_expressions() {
        let tests = vec![
            ("quote(5)", "5"),
            ("quote(5 + 8)", "(5 + 8)"),
            ("quote(foobar)", "Identifier(1)"),
            ("quote(foobar + barfoo)", "(Identifier(1) + Identifier(2))"),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            match result {
                Ok(ir) => match &*ir {
                    IR::Quote(value) => {
                        assert_eq!(expected.to_string(), value.to_string());
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
    fn it_evaluates_recursive_functions() {
        let tests = vec![
            (
                r#"
                let fact = fn(n) {
                    if (n <= 1) {
                        return 1;
                    }
                    return n * fact(n - 1);
                }
                fact(20);
                "#,
                2_432_902_008_176_640_000,
            ),
            (
                r#"
                let fib = fn(n) {
                    if (n <= 0) {
                        return 0;
                    };
                    if (n == 1) {
                        return 1;
                    }
                    return fib(n - 1) + fib(n - 2);
                }
                fib(6);
                "#,
                8,
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
