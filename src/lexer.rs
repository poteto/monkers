use crate::token::{IntegerSize, Token};
use std::{cell::RefCell, rc::Rc};
use string_interner::StringInterner;

#[derive(Debug)]
pub struct Lexer<'a> {
    interner: Rc<RefCell<StringInterner>>,
    input: &'a str,
    position: usize,
    read_position: usize,
    pub ch: Option<char>,
    pub row: usize,
    pub col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, interner: Rc<RefCell<StringInterner>>) -> Self {
        let mut lexer = Lexer {
            interner,
            input,
            position: 0,
            read_position: 0,
            ch: None,
            row: 1,
            col: 0,
        };
        lexer.read_char();
        lexer
    }

    fn peek_char(&self) -> Option<char> {
        if let Some(ch) = self.input.chars().nth(self.read_position) {
            Some(ch)
        } else {
            None
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else if let Some(ch) = self.input.chars().nth(self.read_position) {
            self.ch = Some(ch);
        } else {
            panic!("read out of range character");
        }
        self.position = self.read_position;
        self.read_position += 1;
        self.col += 1;
    }

    fn handle_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            if !ch.is_ascii_whitespace() {
                break;
            }
            if matches!(self.ch, Some('\n') | Some('\r')) {
                self.col = 0;
                self.row += 1;
            }
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while let Some(ch) = self.ch {
            if !ch.is_ascii_alphabetic() {
                break;
            }
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn read_string(&mut self) -> &str {
        let position = self.position + 1;
        self.read_char();
        while let Some(ch) = self.ch {
            if ch == '"' {
                break;
            }
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;
        while let Some(ch) = self.ch {
            if !ch.is_ascii_digit() {
                break;
            }
            self.read_char();
        }
        &self.input[position..self.position]
    }

    pub fn is_end_of_file(&self) -> bool {
        self.read_position >= self.input.len() && self.ch == None
    }

    pub fn next_token(&mut self) -> Token {
        if self.is_end_of_file() {
            return Token::EndOfFile;
        }
        self.handle_whitespace();
        let token: Token;
        match self.ch {
            // '=='
            Some('=') if self.peek_char().unwrap() == '=' => {
                self.read_char(); // consume '=' after first '='
                token = Token::Equal
            }
            Some('=') => token = Token::Assign,
            Some('+') => token = Token::Plus,
            Some('-') => token = Token::Minus,
            // '!='
            Some('!') if self.peek_char().unwrap() == '=' => {
                self.read_char(); // consume '=' after first '!'
                token = Token::NotEqual
            }
            Some('!') => token = Token::Bang,
            Some('*') => token = Token::Asterisk,
            Some('/') => token = Token::Slash,

            Some('<') if self.peek_char().unwrap() == '=' => {
                self.read_char();
                token = Token::LessThanEqual
            }
            Some('<') => token = Token::LessThan,
            Some('>') if self.peek_char().unwrap() == '=' => {
                self.read_char();
                token = Token::GreaterThanEqual
            }
            Some('>') => token = Token::GreaterThan,

            Some(',') => token = Token::Comma,
            Some(':') => token = Token::Colon,
            Some(';') => token = Token::Semicolon,
            Some('(') => token = Token::Lparen,
            Some(')') => token = Token::Rparen,
            Some('{') => token = Token::Lbrace,
            Some('}') => token = Token::Rbrace,
            Some('[') => token = Token::Lbracket,
            Some(']') => token = Token::Rbracket,

            Some('"') => {
                let interner = Rc::clone(&self.interner);
                let literal = self.read_string();
                token = Token::String(interner.borrow_mut().get_or_intern(literal));
            }

            Some(ch) if ch.is_ascii_alphabetic() => {
                let interner = Rc::clone(&self.interner);
                let literal = self.read_identifier();
                // early return as we don't need to call `read_char` again past the `match` statement
                return lookup_identifier(interner, literal);
            }
            Some(ch) if ch.is_ascii_digit() => {
                let literal = self.read_number();
                match literal.parse::<IntegerSize>() {
                    Ok(i) => return Token::Integer(i),
                    Err(error) => panic!("{}", error),
                }
            }
            Some(ch) => token = Token::Illegal(ch),
            None => token = Token::EndOfFile,
        };
        self.read_char();
        token
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_end_of_file() {
            None
        } else {
            Some(self.next_token())
        }
    }
}

fn lookup_identifier(interner: Rc<RefCell<StringInterner>>, identifier: &str) -> Token {
    match identifier {
        "fn" => Token::Function,
        "let" => Token::Let,
        "true" => Token::Boolean(true),
        "false" => Token::Boolean(false),
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        _ => Token::Identifier(interner.borrow_mut().get_or_intern(identifier)),
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::lexer::Lexer;
    use crate::token::Token;
    use string_interner::StringInterner;

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
            Token::EndOfFile,
        ];
        let interner = Rc::new(RefCell::new(StringInterner::default()));
        let mut lexer = Lexer::new(input, Rc::clone(&interner));

        for expect in expected {
            let token = lexer.next_token();
            assert_eq!(expect, token, "Expected `{}`", expect);
        }

        assert!(
            lexer.is_end_of_file(),
            "Postcondition: Lexer should have fully lexed the entire input"
        );
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
        !-/*5;
        5 < 10 > 5;
        5 <= 10 >= 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        "foobar";
        "foo bar";
        [1, 2][0];
        {"foo": "bar"}
        "#;
        let interner = Rc::new(RefCell::new(StringInterner::default()));
        let test_interner = Rc::clone(&interner);
        let mut test_interner = test_interner.borrow_mut();

        let expected = vec![
            Token::Let,
            Token::Identifier(test_interner.get_or_intern("five")),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(test_interner.get_or_intern("ten")),
            Token::Assign,
            Token::Integer(10),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(test_interner.get_or_intern("add")),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Identifier(test_interner.get_or_intern("x")),
            Token::Comma,
            Token::Identifier(test_interner.get_or_intern("y")),
            Token::Rparen,
            Token::Lbrace,
            Token::Identifier(test_interner.get_or_intern("x")),
            Token::Plus,
            Token::Identifier(test_interner.get_or_intern("y")),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(test_interner.get_or_intern("result")),
            Token::Assign,
            Token::Identifier(test_interner.get_or_intern("add")),
            Token::Lparen,
            Token::Identifier(test_interner.get_or_intern("five")),
            Token::Comma,
            Token::Identifier(test_interner.get_or_intern("ten")),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Integer(5),
            Token::Semicolon,
            Token::Integer(5),
            Token::LessThan,
            Token::Integer(10),
            Token::GreaterThan,
            Token::Integer(5),
            Token::Semicolon,
            Token::Integer(5),
            Token::LessThanEqual,
            Token::Integer(10),
            Token::GreaterThanEqual,
            Token::Integer(5),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Integer(5),
            Token::LessThan,
            Token::Integer(10),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::Boolean(true),
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::Boolean(false),
            Token::Semicolon,
            Token::Rbrace,
            Token::Integer(10),
            Token::Equal,
            Token::Integer(10),
            Token::Semicolon,
            Token::Integer(10),
            Token::NotEqual,
            Token::Integer(9),
            Token::Semicolon,
            Token::String(test_interner.get_or_intern("foobar")),
            Token::Semicolon,
            Token::String(test_interner.get_or_intern("foo bar")),
            Token::Semicolon,
            Token::Lbracket,
            Token::Integer(1),
            Token::Comma,
            Token::Integer(2),
            Token::Rbracket,
            Token::Lbracket,
            Token::Integer(0),
            Token::Rbracket,
            Token::Semicolon,
            Token::Lbrace,
            Token::String(test_interner.get_or_intern("foo")),
            Token::Colon,
            Token::String(test_interner.get_or_intern("bar")),
            Token::Rbrace,
            Token::EndOfFile,
        ];
        drop(test_interner);
        let mut lexer = Lexer::new(input, Rc::clone(&interner));

        for expect in expected {
            let token = lexer.next_token();
            assert_eq!(expect, token, "Expected `{}`", expect);
        }

        assert!(
            lexer.is_end_of_file(),
            "Postcondition: Lexer should have fully lexed the entire input"
        );
    }

    #[test]
    fn it_keeps_track_of_row_and_col() {
        let input = r#"let hello = 1;
let world = 2;"#;
        let expected = vec![
            (Some('l'), 1, 1),
            (Some(' '), 1, 4),
            (Some(' '), 1, 10),
            (Some(' '), 1, 12),
            (Some(';'), 1, 14),
            (Some('\n'), 1, 15),
            (Some(' '), 2, 4),
            (Some(' '), 2, 10),
            (Some(' '), 2, 12),
            (Some(';'), 2, 14),
            (None, 2, 15),
        ];
        let interner = Rc::new(RefCell::new(StringInterner::default()));
        let mut lexer = Lexer::new(input, Rc::clone(&interner));

        for expect in expected {
            assert_eq!(expect, (lexer.ch, lexer.row, lexer.col));
            lexer.next_token();
        }

        assert!(
            lexer.is_end_of_file(),
            "Postcondition: Lexer should have fully lexed the entire input"
        );
    }
}
