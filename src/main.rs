use monkers::eval::eval;
use monkers::{lexer::Lexer, parser::Parser};
use rustyline::error::ReadlineError;
use rustyline::Editor;

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
                let lexer = Lexer::new(&line);
                let mut parser = Parser::new(lexer);
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
