use crate::token::{lookup_identifier, IntegerSize, Token};
use std::result;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: char,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0 as char,
        };
        lexer.read_char();
        lexer
    }

    fn peek_char(&self) -> result::Result<char, &str> {
        if self.read_position >= self.input.len() {
            Ok(0 as char)
        } else if let Some(ch) = self.input.chars().nth(self.read_position) {
            Ok(ch)
        } else {
            Err("peeked out of range character")
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0 as char;
        } else if let Some(ch) = self.input.chars().nth(self.read_position) {
            self.ch = ch;
        } else {
            panic!("read out of range character");
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.ch.is_ascii_alphabetic() {
            self.read_char();
        }
        self.input
            .chars()
            .skip(position)
            .take(self.position - position)
            .collect()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        self.input
            .chars()
            .skip(position)
            .take(self.position - position)
            .collect()
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token: Token;
        match self.ch {
            '=' => token = Token::Assign,
            ';' => token = Token::Semicolon,
            '(' => token = Token::Lparen,
            ')' => token = Token::Rparen,
            ',' => token = Token::Comma,
            '+' => token = Token::Plus,
            '{' => token = Token::Lbrace,
            '}' => token = Token::Rbrace,
            '0' => token = Token::EndOfFile,
            _ => {
                if self.ch.is_ascii_alphabetic() {
                    let literal = self.read_identifier();
                    // early return as we don't need to call `read_char` again past the `match` statement
                    return lookup_identifier(literal);
                } else if self.ch.is_ascii_digit() {
                    let literal = self.read_number();
                    match literal.parse::<IntegerSize>() {
                        Ok(i) => return Token::Integer(i),
                        Err(_) => panic!("Could not parse lexed integer literal"),
                    }
                } else {
                    token = Token::Illegal(self.ch)
                }
            }
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn it_lexes_operators_and_delimiters() {
        let input = "=+(){},;";
        let expected = vec![
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace,
            Token::Comma,
            Token::Semicolon,
        ];
        let mut lexer = Lexer::new(input);

        for expect in expected {
            let token = lexer.next_token();
            assert_eq!(expect, token);
        }
    }

    #[test]
    fn it_lexes_additional_operators_and_keywords() {
        let input = r#"
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
        "#;
        let expected = vec![
            Token::Let,
            Token::Identifier(String::from("five")),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("ten")),
            Token::Assign,
            Token::Integer(10),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Identifier(String::from("x")),
            Token::Comma,
            Token::Identifier(String::from("y")),
            Token::Rparen,
            Token::Lbrace,
            Token::Identifier(String::from("x")),
            Token::Plus,
            Token::Identifier(String::from("y")),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("result")),
            Token::Assign,
            Token::Identifier(String::from("add")),
            Token::Lparen,
            Token::Identifier(String::from("five")),
            Token::Comma,
            Token::Identifier(String::from("ten")),
            Token::Rparen,
            Token::Semicolon,
        ];
        let mut lexer = Lexer::new(input);

        for expect in expected {
            let token = lexer.next_token();
            assert_eq!(expect, token);
        }
    }
}
