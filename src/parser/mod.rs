mod error;

use string_interner::symbol::SymbolU32;

pub use crate::parser::error::{ParserError, ParserErrorMessage};
use crate::{
    ast::{
        BlockStatement, BooleanExpression, CallExpression, Expression, FunctionLiteral, Identifier,
        IfExpression, InfixExpression, LetStatement, PrefixExpression, Program, ReturnStatement,
        Statement, StringLiteral,
    },
    lexer::Lexer,
    token::Token,
};
use std::mem;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

fn precedence_for(token: &Token) -> Precedence {
    match token {
        Token::Equal | Token::NotEqual => Precedence::Equals,
        Token::LessThan | Token::GreaterThan => Precedence::LessGreater,
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Slash | Token::Asterisk => Precedence::Product,
        Token::Lparen => Precedence::Call,
        _ => Precedence::Lowest,
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            curr_token: Token::None,
            peek_token: Token::None,
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn next_token(&mut self) -> () {
        mem::swap(&mut self.curr_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    pub fn expect_peek(&mut self, expected_token: Token) -> Result<(), ParserError> {
        if self.peek_token == expected_token {
            self.next_token();
            Ok(())
        } else {
            Err(self.parse_syntax_error(format!(
                "Expected next character to be `{}`, got `{}` instead",
                expected_token, self.peek_token,
            )))
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = Vec::new();
        let mut errors: Vec<ParserError> = Vec::new();
        while self.curr_token != Token::EndOfFile {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => errors.push(error),
            }
            self.next_token();
        }
        Program { statements, errors }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        if let Token::Identifier(ident) = self.peek_token {
            self.next_token(); // let -> identifier
            self.expect_peek(Token::Assign)?;
            self.next_token();
            let statement = LetStatement {
                token: Token::Let,
                name: Identifier(ident),
                value: self.parse_expression(Precedence::Lowest)?,
            };
            if self.peek_token == Token::Semicolon {
                self.next_token();
            }
            Ok(Statement::Let(statement))
        } else {
            Err(self.parse_syntax_error(format!(
                "Expected identifier to follow `{}`, got `{}` instead",
                Token::Let,
                self.peek_token
            )))
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token();
        let statement = ReturnStatement {
            token: Token::Return,
            return_value: self.parse_expression(Precedence::Lowest)?,
        };
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }
        Ok(Statement::Return(statement))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression_statement;
        match self.parse_expression(Precedence::Lowest) {
            Ok(expression) => expression_statement = Ok(Statement::Expression(expression)),
            Err(error) => expression_statement = Err(error),
        }
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }
        expression_statement
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParserError> {
        let mut statements = Vec::new();
        self.next_token();
        while self.curr_token != Token::Rbrace && self.curr_token != Token::EndOfFile {
            if let Ok(statement) = self.parse_statement() {
                statements.push(statement)
            }
            self.next_token();
        }
        Ok(BlockStatement {
            token: Token::Lbrace,
            statements,
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let mut left_expression = self.parse_prefix();
        if left_expression.is_err() {
            return left_expression;
        }
        while self.peek_token != Token::Semicolon && precedence < precedence_for(&self.peek_token) {
            self.next_token();
            let infix = self.parse_infix(left_expression?.clone());
            left_expression = infix;
        }
        left_expression
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();
        let expression = self.parse_expression(Precedence::Lowest);
        self.expect_peek(Token::Rparen)?;
        expression
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek(Token::Lparen)?;
        self.next_token();
        if let Ok(condition) = self.parse_expression(Precedence::Lowest) {
            self.expect_peek(Token::Rparen)?;
            self.expect_peek(Token::Lbrace)?;
            let mut expression = IfExpression {
                token: Token::If,
                condition: Box::new(condition),
                consequence: Default::default(),
                alternative: Default::default(),
            };
            if let Ok(statement) = self.parse_block_statement() {
                expression.consequence = Some(statement);
            }
            if self.peek_token == Token::Else {
                self.next_token();
                self.expect_peek(Token::Lbrace)?;
                if let Ok(statement) = self.parse_block_statement() {
                    expression.alternative = Some(statement)
                }
            }
            Ok(Expression::If(expression))
        } else {
            Err(self.parse_syntax_error(format!("Expected a condition expression",)))
        }
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek(Token::Lparen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_peek(Token::Lbrace)?;
        let body = self.parse_block_statement()?;
        Ok(Expression::Function(FunctionLiteral {
            token: Token::Function,
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, ParserError> {
        let mut identifiers: Vec<Identifier> = Vec::new();
        if self.peek_token == Token::Rparen {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();
        let identifier = self.expect_identifier()?;
        identifiers.push(Identifier(identifier));

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            let identifier = self.expect_identifier()?;
            identifiers.push(Identifier(identifier));
        }

        self.expect_peek(Token::Rparen)?;
        Ok(identifiers)
    }

    fn parse_prefix(&mut self) -> Result<Expression, ParserError> {
        match &self.curr_token {
            Token::Identifier(identifier_key) => {
                Ok(Expression::Identifier(Identifier(*identifier_key)))
            }
            Token::Integer(i) => Ok(Expression::Integer(*i)),
            Token::Boolean(_) => Ok(Expression::Boolean(BooleanExpression {
                token: self.curr_token.clone(),
                value: self.curr_token == Token::Boolean(true),
            })),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::Lparen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            Token::String(string_key) => Ok(Expression::String(StringLiteral {
                token: self.curr_token.clone(),
                value: *string_key,
            })),
            _ => Err(ParserError::UnhandledPrefixOperator(
                self.curr_token.clone(),
            )),
        }
    }

    fn parse_infix(&mut self, left: Expression) -> Result<Expression, ParserError> {
        match &self.curr_token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Equal
            | Token::NotEqual
            | Token::LessThan
            | Token::GreaterThan => self.parse_infix_expression(left),
            Token::Lparen => self.parse_call_expression(left),
            _ => Err(ParserError::UnhandledInfixOperator(self.curr_token.clone())),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let token = self.curr_token.clone();
        self.next_token();
        Ok(Expression::Prefix(PrefixExpression {
            token,
            right: Box::new(self.parse_expression(Precedence::Prefix)?),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        let token = self.curr_token.clone();
        let precedence = precedence_for(&token);
        self.next_token();
        Ok(Expression::Infix(InfixExpression {
            token,
            left: Box::new(left),
            right: Box::new(self.parse_expression(precedence)?),
        }))
    }

    fn parse_call_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        Ok(Expression::Call(CallExpression {
            token: Token::Lparen,
            function: Box::new(left),
            arguments: self.parse_call_arguments()?,
        }))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut arguments: Vec<Expression> = Vec::new();
        if self.peek_token == Token::Rparen {
            self.next_token();
            return Ok(arguments);
        }

        self.next_token();
        arguments.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            arguments.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(Token::Rparen)?;
        Ok(arguments)
    }

    fn parse_syntax_error(&self, message: String) -> ParserError {
        ParserError::SyntaxError(ParserErrorMessage {
            message,
            row: self.lexer.row,
            col: self.lexer.col,
        })
    }

    fn expect_identifier(&self) -> Result<SymbolU32, ParserError> {
        if let Token::Identifier(identifier) = self.curr_token {
            Ok(identifier)
        } else {
            Err(self.parse_syntax_error(format!("Expected an identifier, got {}", self.curr_token)))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Program;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use std::{cell::RefCell, rc::Rc};
    use string_interner::StringInterner;

    fn test_parse(input: &str) -> Program {
        let interner = Rc::new(RefCell::new(StringInterner::default()));
        let lexer = Lexer::new(input, Rc::clone(&interner));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        for error in &program.errors {
            eprintln!("{}", error);
        }

        program
    }

    #[test]
    fn it_parses_let_statements() {
        let tests = vec![
            ("let x = 5;", "let Identifier(0) = 5;"),
            ("let y = 10;", "let Identifier(0) = 10;"),
            ("let foobar = 838383;", "let Identifier(0) = 838383;"),
        ];

        for (input, expected_string) in tests {
            let program = test_parse(input);

            assert_eq!(expected_string, program.to_string());
            assert!(!program.statements.is_empty());
            assert!(program.errors.is_empty());
        }
    }

    #[test]
    fn it_parses_if_statements() {
        let tests = vec![(
            "if (x < y) { x } else { y }",
            "if (Identifier(0) < Identifier(1)) Identifier(0) else Identifier(1)",
        )];

        for (input, expected_string) in tests {
            let program = test_parse(input);

            assert_eq!(expected_string, program.to_string());
            assert!(!program.statements.is_empty());
            assert!(program.errors.is_empty());
        }
    }

    #[test]
    fn it_handles_parser_errors() {
        let input = r#"
        let x 5;
        let = 10;
        let 838383;
        "#;
        let expected = vec![
            "[Row: 2, Col: 16] Expected next character to be `=`, got `5` instead",
            "[Row: 3, Col: 14] Expected identifier to follow `let`, got `=` instead",
            "Unhandled prefix operator: `=`",
            "[Row: 4, Col: 19] Expected identifier to follow `let`, got `838383` instead",
        ];

        let program = test_parse(input);
        for (i, expect) in expected.iter().enumerate() {
            assert_eq!(expect, &program.errors[i].to_string());
        }

        assert_eq!(program.statements.len(), 3);
        assert_eq!(program.errors.len(), 4);
    }

    #[test]
    fn it_parses_return_statements() {
        let tests = vec![
            ("return 5;", "return 5;"),
            ("return 10;", "return 10;"),
            ("return 993322;", "return 993322;"),
        ];

        for (input, expected_string) in tests {
            let program = test_parse(input);

            assert_eq!(expected_string, program.to_string());
            assert!(!program.statements.is_empty());
            assert!(program.errors.is_empty());
        }
    }

    #[test]
    fn it_parses_numbers() {
        let tests = vec![("5;", "5")];

        for (input, expected_string) in tests {
            let program = test_parse(input);

            assert_eq!(expected_string, program.to_string());
            assert!(!program.statements.is_empty());
            assert!(program.errors.is_empty());
        }
    }

    #[test]
    fn it_parses_valid_prefix_expressions() {
        let tests = vec![
            ("!5;", "(!5)"),
            ("-15;", "(-15)"),
            ("!true", "(!true)"),
            ("!false", "(!false)"),
        ];

        for (input, expected_string) in tests {
            let program = test_parse(input);

            assert_eq!(expected_string, program.to_string());
            assert!(!program.statements.is_empty());
            assert!(program.errors.is_empty());
        }
    }

    #[test]
    fn it_handles_invalid_prefix_expressions() {
        let tests = vec![
            ("@5;", "Unhandled prefix operator: `Illegal(@)`"),
            ("#5;", "Unhandled prefix operator: `Illegal(#)`"),
            ("$5;", "Unhandled prefix operator: `Illegal($)`"),
            ("%5;", "Unhandled prefix operator: `Illegal(%)`"),
            ("^5;", "Unhandled prefix operator: `Illegal(^)`"),
            ("&5;", "Unhandled prefix operator: `Illegal(&)`"),
        ];

        for (input, expected_string) in tests {
            let program = test_parse(input);

            assert!(program.errors.len() == 1);
            assert_eq!(expected_string, program.errors.first().unwrap().to_string());
        }
    }

    #[test]
    fn it_parses_valid_infix_expressions() {
        let tests = vec![
            ("5 + 5;", "(5 + 5)"),
            ("5 - 5;", "(5 - 5)"),
            ("5 * 5;", "(5 * 5)"),
            ("5 / 5;", "(5 / 5)"),
            ("5 > 5;", "(5 > 5)"),
            ("5 < 5;", "(5 < 5)"),
            ("5 == 5;", "(5 == 5)"),
            ("5 != 5;", "(5 != 5)"),
            ("true == true;", "(true == true)"),
            ("true != true;", "(true != true)"),
            ("false == false;", "(false == false)"),
        ];

        for (input, expected_string) in tests {
            let program = test_parse(input);

            assert_eq!(expected_string, program.to_string());
            assert!(!program.statements.is_empty());
            assert!(program.errors.is_empty());
        }
    }

    #[test]
    fn it_parses_operator_precedence() {
        let tests = vec![
            ("-a * b;", "((-Identifier(0)) * Identifier(1))"),
            ("!-a;", "(!(-Identifier(0)))"),
            ("a + b + c;", "((Identifier(0) + Identifier(1)) + Identifier(2))"),
            ("a + b - c;", "((Identifier(0) + Identifier(1)) - Identifier(2))"),
            ("a * b * c;", "((Identifier(0) * Identifier(1)) * Identifier(2))"),
            ("a * b / c;", "((Identifier(0) * Identifier(1)) / Identifier(2))"),
            ("a + b / c;", "(Identifier(0) + (Identifier(1) / Identifier(2)))"),
            ("a + b * c + d / e - f;", "(((Identifier(0) + (Identifier(1) * Identifier(2))) + (Identifier(3) / Identifier(4))) - Identifier(5))"),
            ("3 + 4; -5 * 5;", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5;",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true;", "true"),
            ("false;", "false"),
            ("3 > 5 == false;", "((3 > 5) == false)"),
            ("3 < 5 == true;", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4;", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2;", "((5 + 5) * 2)"),
            ("2 / (5 + 5);", "(2 / (5 + 5))"),
            ("-(5 + 5);", "(-(5 + 5))"),
            ("!(true == true);", "(!(true == true))"),
        ];

        for (input, expected_string) in tests {
            let program = test_parse(input);

            assert_eq!(expected_string, program.to_string());
            assert!(!program.statements.is_empty());
            assert!(program.errors.is_empty());
        }
    }

    #[test]
    fn it_parses_function_literals() {
        let tests = vec![
            (
                "fn(x, y) { x + y; };",
                "fn(Identifier(0), Identifier(1)) {(Identifier(0) + Identifier(1))}",
            ),
            ("fn() {};", "fn() {}"),
            ("fn(x) {};", "fn(Identifier(0)) {}"),
            (
                "fn(x, y, z) {};",
                "fn(Identifier(0), Identifier(1), Identifier(2)) {}",
            ),
        ];

        for (input, expected_string) in tests {
            let program = test_parse(input);

            assert_eq!(expected_string, program.to_string());
            assert!(!program.statements.is_empty());
            assert!(program.errors.is_empty());
        }
    }

    #[test]
    fn it_parses_call_expressions() {
        let tests = vec![
            ("add(1, 2 * 3, 4 + 5);", "Identifier(0)(1, (2 * 3), (4 + 5))"),
            ("a + add(b * c) + d;", "((Identifier(0) + Identifier(1)((Identifier(2) * Identifier(3)))) + Identifier(4))"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));",
                "Identifier(0)(Identifier(1), Identifier(2), 1, (2 * 3), (4 + 5), Identifier(0)(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g); ",
                "Identifier(0)((((Identifier(1) + Identifier(2)) + ((Identifier(3) * Identifier(4)) / Identifier(5))) + Identifier(6)))",
            ),
        ];

        for (input, expected_string) in tests {
            let program = test_parse(input);

            assert_eq!(expected_string, program.to_string());
            assert!(!program.statements.is_empty());
            assert!(program.errors.is_empty());
        }
    }

    #[test]
    fn it_parses_string_literals() {
        let tests = vec![("\"hello world\";", "String(0)")];

        for (input, expected_string) in tests {
            let program = test_parse(input);

            println!("{:#?}", program);

            assert_eq!(expected_string, program.to_string());
            assert!(!program.statements.is_empty());
            assert!(program.errors.is_empty());
        }
    }
}
