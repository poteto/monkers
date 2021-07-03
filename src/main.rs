use monkers::eval::{Env, Interpreter};
use monkers::{lexer::Lexer, parser::Parser};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use string_interner::StringInterner;

use std::{cell::RefCell, rc::Rc};
fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let interner = Rc::new(RefCell::new(StringInterner::default()));
    let env = Rc::new(RefCell::new(Env::new()));
    let interpreter = Interpreter::new(env, Rc::clone(&interner));
    loop {
        let readline = rl.readline("🐒 >> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let lexer = Lexer::new(&line, Rc::clone(&interner));
                let mut parser = Parser::new(lexer);
                let program = parser.parse_program();

                for error in &program.errors {
                    eprintln!("{}", error);
                }

                match interpreter.eval(&program) {
                    Ok(ir) => println!("{}", ir),
                    Err(error) => eprintln!("{}", error),
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
