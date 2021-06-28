mod error;

pub use crate::parser::error::{ParserError, ParserErrorMessage};
use crate::{
    ast::{
        BlockStatement, BooleanExpression, Expression, FunctionLiteral, Identifier, IfExpression,
        InfixExpression, LetStatement, PrefixExpression, Program, ReturnStatement, Statement,
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
    _Call,
}

fn precedence_for(token: &Token) -> Precedence {
    match token {
        Token::Equal | Token::NotEqual => Precedence::Equals,
        Token::LessThan | Token::GreaterThan => Precedence::LessGreater,
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Slash | Token::Asterisk => Precedence::Product,
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
        self.next_token();
        if let Token::Identifier(ident) = self.curr_token.clone() {
            if self.peek_token != Token::Assign {
                return Err(self.parse_syntax_error(format!(
                    "Expected `{}` to follow `{}`, got `{}` instead",
                    Token::Assign,
                    Token::Let,
                    self.peek_token
                )));
            }
            let statement = LetStatement {
                token: Token::Let,
                name: Identifier(ident.to_string()),
            };
            // TODO: Skip expressions until we encounter a semicolon
            while self.curr_token != Token::Semicolon {
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
        };
        while self.curr_token != Token::Semicolon {
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
        let token = self.curr_token.clone();
        let mut statements = Vec::new();
        self.next_token();
        while self.curr_token != Token::Rbrace && self.curr_token != Token::EndOfFile {
            if let Ok(statement) = self.parse_statement() {
                statements.push(statement)
            }
            self.next_token();
        }
        Ok(BlockStatement { token, statements })
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
        let token = self.curr_token.clone();
        self.expect_peek(Token::Lparen)?;

        self.next_token();
        if let Ok(condition) = self.parse_expression(Precedence::Lowest) {
            let condition = Some(Box::new(condition));
            self.expect_peek(Token::Rparen)?;
            self.expect_peek(Token::Lbrace)?;

            let mut expression = IfExpression {
                token,
                condition,
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
        let token = self.curr_token.clone();
        self.expect_peek(Token::Lparen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_peek(Token::Lbrace)?;
        let body = self.parse_block_statement()?;
        Ok(Expression::Function(FunctionLiteral {
            token,
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
        identifiers.push(Identifier(self.curr_token.to_string()));

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            identifiers.push(Identifier(self.curr_token.to_string()));
        }

        self.expect_peek(Token::Rparen)?;
        Ok(identifiers)
    }

    fn parse_prefix(&mut self) -> Result<Expression, ParserError> {
        match &self.curr_token {
            Token::Identifier(ident) => Ok(Expression::Identifier(Identifier(ident.to_string()))),
            Token::Integer(i) => Ok(Expression::Integer(*i)),
            Token::Boolean(_) => Ok(Expression::Boolean(BooleanExpression {
                token: self.curr_token.clone(),
                value: self.curr_token == Token::Boolean(true),
            })),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::Lparen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
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
            _ => Err(ParserError::UnhandledInfixOperator(self.curr_token.clone())),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let token = self.curr_token.clone();
        let operator = self.curr_token.to_string();
        self.next_token();
        Ok(Expression::Prefix(PrefixExpression {
            token,
            operator,
            right: Box::new(self.parse_expression(Precedence::Prefix)?),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        let token = self.curr_token.clone();
        let operator = self.curr_token.to_string();
        let precedence = precedence_for(&self.curr_token);
        self.next_token();
        Ok(Expression::Infix(InfixExpression {
            token,
            operator,
            left: Box::new(left),
            right: Box::new(self.parse_expression(precedence)?),
        }))
    }

    fn parse_syntax_error(&self, message: String) -> ParserError {
        ParserError::SyntaxError(ParserErrorMessage {
            message,
            row: self.lexer.row,
            col: self.lexer.col,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn it_parses_let_statements() {
        let input = r#"let x = 5;
let y = 10;
let foobar = 838383;"#;
        let expected = vec!["let x = TODO;", "let y = TODO;", "let foobar = TODO;"].join("");
        let expected_len = input.trim().lines().count();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            program.statements.len(),
            expected_len,
            "Expected {} statements",
            expected_len
        );
        assert!(program.errors.is_empty());
        assert_eq!(program.to_string(), expected);
    }

    #[test]
    fn it_parses_if_statements() {
        let input = "if (x < y) { x } else { y }";
        let expected_len = 1;
        let expected = "if (x < y) x else y";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            program.statements.len(),
            expected_len,
            "Expected {} statements",
            expected_len
        );

        assert!(program.errors.is_empty());
        assert_eq!(program.to_string(), expected);
    }

    #[test]
    fn it_handles_parser_errors() {
        let input = r#"let x 5;
let = 10;
let 838383;"#;
        let expected = vec![
            "[Row: 1, Col: 8] Expected `=` to follow `let`, got `5` instead",
            "[Row: 2, Col: 9] Expected identifier to follow `let`, got `10` instead",
            "[Row: 3, Col: 12] Expected identifier to follow `let`, got `;` instead",
        ];
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        for (i, expect) in expected.iter().enumerate() {
            assert_eq!(expect, &program.errors[i].to_string());
        }

        assert_eq!(program.statements.len(), 2);
        assert_eq!(program.errors.len(), 4);
    }

    #[test]
    fn it_parses_return_statements() {
        let input = r#"return 5;
return 10;
return 993322;"#;
        let expected = vec!["return TODO;", "return TODO;", "return TODO;"].join("");
        let expected_len = input.trim().lines().count();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            program.statements.len(),
            expected_len,
            "Expected {} statements",
            expected_len
        );
        assert!(program.errors.is_empty());
        assert_eq!(program.to_string(), expected);
    }

    #[test]
    fn it_parses_numbers() {
        let input = "5;";
        let expected = "5";
        let expected_len = 1;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            program.statements.len(),
            expected_len,
            "Expected {} statements",
            expected_len
        );
        assert!(program.errors.is_empty());
        assert_eq!(program.to_string(), expected);
    }

    #[test]
    fn it_parses_valid_prefix_expressions() {
        let tests = vec![
            ("!5;", "!", "(!5)"),
            ("-15;", "-", "(-15)"),
            ("!true", "!", "(!true)"),
            ("!false", "!", "(!false)"),
        ];

        for (input, expected_operator, expected_string) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert_eq!(program.statements.len(), 1);
            assert!(program.errors.is_empty());

            let statement = &program.statements[0];
            if let Some(statement_operator) = statement.token() {
                assert_eq!(expected_operator, statement_operator.to_string());
            }

            assert_eq!(expected_string, statement.to_string());
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
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert!(program.errors.len() == 1);
            assert_eq!(expected_string, &program.errors[0].to_string());
        }
    }

    #[test]
    fn it_parses_valid_infix_expressions() {
        let tests = vec![
            ("5 + 5;", "+", "(5 + 5)"),
            ("5 - 5;", "-", "(5 - 5)"),
            ("5 * 5;", "*", "(5 * 5)"),
            ("5 / 5;", "/", "(5 / 5)"),
            ("5 > 5;", ">", "(5 > 5)"),
            ("5 < 5;", "<", "(5 < 5)"),
            ("5 == 5;", "==", "(5 == 5)"),
            ("5 != 5;", "!=", "(5 != 5)"),
            ("true == true;", "==", "(true == true)"),
            ("true != true;", "!=", "(true != true)"),
            ("false == false;", "==", "(false == false)"),
        ];

        for (input, expected_operator, expected_string) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert_eq!(program.statements.len(), 1);
            assert!(program.errors.is_empty());

            let statement = &program.statements[0];
            if let Some(statement_operator) = statement.token() {
                assert_eq!(expected_operator, statement_operator.to_string());
            }

            assert_eq!(expected_string, statement.to_string());
        }
    }

    #[test]
    fn it_parses_operator_precedence() {
        let tests = vec![
            ("-a * b;", "((-a) * b)"),
            ("!-a;", "(!(-a))"),
            ("a + b + c;", "((a + b) + c)"),
            ("a + b - c;", "((a + b) - c)"),
            ("a * b * c;", "((a * b) * c)"),
            ("a * b / c;", "((a * b) / c)"),
            ("a + b / c;", "(a + (b / c))"),
            ("a + b * c + d / e - f;", "(((a + (b * c)) + (d / e)) - f)"),
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
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            if !program.errors.is_empty() {
                eprintln!("{:?}", program.errors);
            }

            assert_eq!(expected_string, program.to_string());
            assert!(program.statements.len() != 0);
            assert!(program.errors.is_empty());
        }
    }

    #[test]
    fn it_parses_function_literals() {
        let tests = vec![
            (
                "fn(x, y) { x + y; };",
                "fn(Identifier(x), Identifier(y)) {(x + y)}",
            ),
            ("fn() {};", "fn() {}"),
            ("fn(x) {};", "fn(Identifier(x)) {}"),
            (
                "fn(x, y, z) {};",
                "fn(Identifier(x), Identifier(y), Identifier(z)) {}",
            ),
        ];

        for (input, expected_string) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            if !program.errors.is_empty() {
                eprintln!("{:?}", program.errors);
            }

            assert_eq!(expected_string, program.to_string());
            assert!(program.statements.len() != 0);
            assert!(program.errors.is_empty());
        }
    }
}
