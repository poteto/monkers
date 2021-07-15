use std::rc::Rc;

use crate::{ast::Program, code::Instructions, eval::ir::IR};

pub struct Bytecode {
    instructions: Rc<Instructions>,
    constants: Rc<Vec<IR>>,
}

pub struct Compiler {
    instructions: Rc<Instructions>,
    constants: Rc<Vec<IR>>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Rc::new([]),
            constants: Default::default(),
        }
    }

    fn compile(&self, program: &Program) -> Result<(), ()> {
        todo!();
    }

    pub fn to_bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: Rc::clone(&self.instructions),
            constants: Rc::clone(&self.constants),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::Program,
        code::{make, Opcode},
        lexer::Lexer,
        parser::Parser,
    };
    use std::{cell::RefCell, rc::Rc};
    use string_interner::StringInterner;

    use super::*;

    fn test_parse(input: &str) -> Program {
        let interner = Rc::new(RefCell::new(StringInterner::default()));
        let lexer = Lexer::new(input, Rc::clone(&interner));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        for error in &program.errors {
            eprintln!("{}", error);
        }
        program
    }

    fn test_compile(input: &str) -> Result<Bytecode, ()> {
        let program = test_parse(input);
        let compiler = Compiler::new();
        compiler.compile(&program)?;
        Ok(compiler.to_bytecode())
    }

    #[test]
    fn it_makes_bytecode_instructions() {
        let tests = vec![(
            "1 + 2",
            [1, 2],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
            ],
        )];

        for (input, expected_constants, expected_instructions) in tests {
            let bytecode = test_compile(input).unwrap_or_else(|_| panic!("oops"));
            // assert_eq!(bytecode.instructions, &expected_instructions);
            // assert_eq!(bytecode.constants, &expected_constants);
        }
    }
}
