use clap::{App, Arg};
use monkers::{
    ast::Program,
    compiler::{Bytecode, Compiler},
    eval::{Env, Interpreter},
    ir::IR,
    vm::VM,
    {lexer::Lexer, parser::Parser},
};
use rustyline::{error::ReadlineError, Editor};
use string_interner::StringInterner;

use std::{cell::RefCell, rc::Rc, str::FromStr};

const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");

#[derive(Debug)]
enum EvalStrategy {
    Compiled,
    Interpreted,
}

impl FromStr for EvalStrategy {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "compiled" => Ok(EvalStrategy::Compiled),
            "interpreted" => Ok(EvalStrategy::Interpreted),
            opt => Err(format!("Unsupported option {}", opt)),
        }
    }
}

struct ReplOptions<'strategy> {
    strategy: &'strategy EvalStrategy,
    debug: bool,
}

struct Repl<'strategy> {
    interner: Rc<RefCell<StringInterner>>,
    env: Rc<RefCell<Env>>,
    options: ReplOptions<'strategy>,
}

impl<'strategy> Repl<'strategy> {
    pub fn new(options: ReplOptions<'strategy>) -> Self {
        Self {
            interner: Rc::new(RefCell::new(StringInterner::default())),
            env: Rc::new(RefCell::new(Env::default())),
            options,
        }
    }

    fn parse(&self, input: &str) -> Program {
        let lexer = Lexer::new(input, Rc::clone(&self.interner));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        for error in &program.errors {
            eprintln!("{}", error);
        }
        program
    }

    fn compile(&self, program: &Program) -> Bytecode {
        let mut compiler = Compiler::new(Rc::clone(&self.interner));
        if let Err(err) = compiler.compile(program) {
            eprintln!("{:?}", err);
        }
        compiler.to_bytecode()
    }

    pub fn eval(&self, input: &str) -> Option<IR> {
        let program = self.parse(input);
        match self.options.strategy {
            EvalStrategy::Compiled => {
                let bytecode = self.compile(&program);
                if self.options.debug {
                    dbg!(&bytecode);
                }
                let mut vm = VM::new(bytecode);
                if let Err(err) = vm.run() {
                    eprintln!("{:?}", err);
                }
                Some(vm.last_popped_stack_element())
            }
            EvalStrategy::Interpreted => {
                let mut interpreter =
                    Interpreter::new(Rc::clone(&self.env), Rc::clone(&self.interner));
                match interpreter.eval(&program) {
                    Ok(ir) => Some((*ir).clone()),
                    Err(error) => {
                        eprintln!("{}", error);
                        None
                    }
                }
            }
        }
    }
}

fn main() {
    let matches = App::new("monkers-repl")
        .version(VERSION)
        .author(AUTHORS)
        .about("The REPL for Monkers, a Monkeylang engine")
        .arg(
            Arg::new("strategy")
                .short('s')
                .long("strategy")
                .about("Which eval strategy to use")
                .takes_value(true)
                .possible_values(&["compiled", "interpreted"])
                .default_value("compiled")
                .required(false),
        )
        .arg(
            Arg::new("debug")
                .short('d')
                .long("debug")
                .about("Whether to print debug statements during evaluation")
                .required(false),
        )
        .get_matches();
    let strategy = matches.value_of_t("strategy").unwrap_or_else(|e| e.exit());
    let debug = matches.is_present("debug");
    println!("Evaluating with strategy: {:?}", &strategy);
    let repl = Repl::new(ReplOptions {
        strategy: &strategy,
        debug,
    });

    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline("🐒 >> ");
        match readline {
            Ok(input) => {
                rl.add_history_entry(input.as_str());
                if let Some(ir) = repl.eval(&input) {
                    println!("{}", ir)
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
