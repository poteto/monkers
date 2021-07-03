use string_interner::StringInterner;
use monkers::eval::eval;
use monkers::{lexer::Lexer, parser::Parser};
use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::{rc::Rc, cell::RefCell};
fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline("🐒 >> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let interner = Rc::new(RefCell::new(StringInterner::default()));
                let lexer = Lexer::new(&line, interner.clone());
                let mut parser = Parser::new(lexer, interner.clone());
                let program = parser.parse_program();

                for error in &program.errors {
                    eprintln!("{}", error);
                }

                match eval(&program) {
                    Ok(ir) => println!("{}", ir),
                    Err(_) => eprintln!("Something went wrong!"),
                };
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
