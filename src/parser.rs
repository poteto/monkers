use crate::ast;
use crate::lexer::Lexer;
use crate::token::Token;
use std::{fmt, mem};

#[derive(Debug, PartialEq)]
pub enum ParserError {
    SyntaxError(ParserErrorMessage),
    UnhandledToken(Token),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::UnhandledToken(token) => write!(f, "Unhandled token: `{}`", token),
            ParserError::SyntaxError(error) => error.fmt(f),
        }
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct ParserErrorMessage {
    message: String,
    row: usize,
    col: usize,
}

impl fmt::Display for ParserErrorMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[Row: {}, Col: {}] {}", self.row, self.col, self.message)
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
        // https://stackoverflow.com/a/39329993
        // swap curr and peek pointers
        mem::swap(&mut self.curr_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut statements: Vec<ast::Statement> = Vec::new();
        let mut errors: Vec<ParserError> = Vec::new();
        while self.curr_token != Token::EndOfFile {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => errors.push(error),
            }
            self.next_token();
        }
        ast::Program { statements, errors }
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParserError> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            _ => Err(ParserError::UnhandledToken(self.curr_token.clone())),
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.next_token();
        if let Token::Identifier(ident) = &self.curr_token {
            if self.peek_token != Token::Assign {
                return Err(ParserError::SyntaxError(ParserErrorMessage {
                    message: format!(
                        "Expected `{}` to follow `{}`, got `{}` instead",
                        Token::Assign,
                        Token::Let,
                        self.peek_token
                    ),
                    col: self.lexer.col,
                    row: self.lexer.row,
                }));
            }
            let statement = ast::LetStatement {
                name: ast::Identifier(ident.to_string()),
            };
            // TODO: Skip expressions until we encounter a semicolon
            while self.curr_token != Token::Semicolon {
                self.next_token();
            }
            Ok(ast::Statement::Let(statement))
        } else {
            Err(ParserError::SyntaxError(ParserErrorMessage {
                message: format!(
                    "Expected identifier to follow `{}`, got `{}` instead",
                    Token::Let,
                    self.peek_token
                ),
                row: self.lexer.row,
                col: self.lexer.col,
            }))
        }
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
        assert_eq!(program.to_string(), expected);
    }

    #[test]
    fn it_handles_parser_errors() {
        let input = r#"let x 5;
let = 10;
let 838383;"#;
        let expected = vec![
            "[Row: 1, Col: 8] Expected `=` to follow `let`, got `5` instead",
            "Unhandled token: `5`",
            "Unhandled token: `;`",
            "[Row: 2, Col: 9] Expected identifier to follow `let`, got `10` instead",
            "Unhandled token: `10`",
            "Unhandled token: `;`",
            "[Row: 3, Col: 12] Expected identifier to follow `let`, got `;` instead",
            "Unhandled token: `;`",
        ];
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        for (i, expect) in expected.iter().enumerate() {
            assert_eq!(expect, &program.errors[i].to_string());
        }

        assert_eq!(program.statements.len(), 0);
    }
}
