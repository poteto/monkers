use crate::token::{lookup_identifier, IntegerSize, Token};

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    pub ch: char,
    pub row: usize,
    pub col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '0',
            row: 1,
            col: 0,
        };
        lexer.read_char();
        lexer
    }

    fn peek_char(&self) -> Result<char, &str> {
        if self.read_position >= self.input.len() {
            Ok('0')
        } else if let Some(ch) = self.input.chars().nth(self.read_position) {
            Ok(ch)
        } else {
            Err("peeked out of range character")
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '0';
        } else if let Some(ch) = self.input.chars().nth(self.read_position) {
            self.ch = ch;
        } else {
            panic!("read out of range character");
        }
        self.position = self.read_position;
        self.read_position += 1;
        self.col += 1;
    }

    fn handle_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            if self.ch == '\n' || self.ch == '\r' {
                self.col = 0;
                self.row += 1;
            }
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

    pub fn is_end_of_file(&self) -> bool {
        self.read_position >= self.input.len() && self.ch == '0'
    }

    pub fn next_token(&mut self) -> Token {
        if self.is_end_of_file() {
            return Token::EndOfFile;
        }
        self.handle_whitespace();
        let token: Token;
        match self.ch {
            '0' => token = Token::EndOfFile,

            // '=='
            '=' if self.peek_char().unwrap() == '=' => {
                self.read_char(); // consume '=' after first '='
                token = Token::Equal
            }
            '=' => token = Token::Assign,
            '+' => token = Token::Plus,
            '-' => token = Token::Minus,
            // '!='
            '!' if self.peek_char().unwrap() == '=' => {
                self.read_char(); // consume '=' after first '!'
                token = Token::NotEqual
            }
            '!' => token = Token::Bang,
            '*' => token = Token::Asterisk,
            '/' => token = Token::Slash,

            '<' => token = Token::LessThan,
            '>' => token = Token::GreaterThan,

            ',' => token = Token::Comma,
            ';' => token = Token::Semicolon,
            '(' => token = Token::Lparen,
            ')' => token = Token::Rparen,
            '{' => token = Token::Lbrace,
            '}' => token = Token::Rbrace,

            _ if self.ch.is_ascii_alphabetic() => {
                let literal = self.read_identifier();
                // early return as we don't need to call `read_char` again past the `match` statement
                return lookup_identifier(literal);
            }
            _ if self.ch.is_ascii_digit() => {
                let literal = self.read_number();
                match literal.parse::<IntegerSize>() {
                    Ok(i) => return Token::Integer(i),
                    Err(_) => panic!("Could not parse lexed integer literal"),
                }
            }
            _ => token = Token::Illegal(self.ch),
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
            Token::EndOfFile,
        ];
        let mut lexer = Lexer::new(input);

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
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;"#;
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
            Token::EndOfFile,
        ];
        let mut lexer = Lexer::new(input);

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
        let expected: Vec<(char, usize, usize)> = vec![
            ('l', 1, 1),
            (' ', 1, 4),
            (' ', 1, 10),
            (' ', 1, 12),
            (';', 1, 14),
            ('\n', 1, 15),
            (' ', 2, 4),
            (' ', 2, 10),
            (' ', 2, 12),
            (';', 2, 14),
            ('0', 2, 15),
        ];
        let mut lexer = Lexer::new(input);

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
