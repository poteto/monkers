use crate::ast;
use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;

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
        while self.curr_token != Token::EndOfFile {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }
            self.next_token();
        }
        ast::Program { statements }
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
        if let Token::Identifier(ident) = &self.peek_token {
            let statement = ast::LetStatement {
                name: ast::Identifier(ident.to_string()),
            };
            // TODO: Skip expressions until we encounter a semicolon
            while self.curr_token != Token::Semicolon {
                self.next_token();
            }
            Some(ast::Statement::Let(statement))
        } else {
            // Parser error: Expected identifier to follow LetStatement
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn it_parses_let_statements() {
        let input = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;
        let expected = vec![
            "let x = TODO;",
            "let y = TODO;",
            "let foobar = TODO;",
        ].join("");

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
}
