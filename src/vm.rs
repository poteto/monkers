use std::convert::TryFrom;

use crate::{
    code::{self, Opcode},
    compiler::Bytecode,
    ir::IR,
};

const STACK_SIZE: usize = 2048;

pub enum VMError {
    NotImplementedYet,
    StackOverflow,
}

pub struct VM {
    bytecode: Bytecode,
    stack: Vec<IR>,
    stack_ptr: usize,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            bytecode,
            stack: Vec::with_capacity(STACK_SIZE),
            stack_ptr: Default::default(),
        }
    }

    pub fn run(&mut self) -> Result<(), VMError> {
        let mut instruction_ptr = 0;
        while instruction_ptr < self.bytecode.instructions.len() {
            match Opcode::try_from(self.bytecode.instructions[instruction_ptr]) {
                Ok(Opcode::OpConstant) => {
                    let const_index =
                        code::read_u16(&self.bytecode.instructions[(instruction_ptr + 1)..]);
                    instruction_ptr += 2;
                    self.push_index(const_index.into())?;
                }
                Ok(Opcode::OpAdd) => {
                    let right = self.pop();
                    let left = self.pop();
                    match (left, right) {
                        (IR::Integer(left_value), IR::Integer(right_value)) => {
                            self.push(IR::Integer(left_value + right_value))?;
                        }
                        _ => return Err(VMError::NotImplementedYet),
                    };
                }
                _ => return Err(VMError::NotImplementedYet),
            };
            instruction_ptr += 1;
        }
        Ok(())
    }

    fn push(&mut self, ir: IR) -> Result<(), VMError> {
        if self.stack_ptr >= STACK_SIZE {
            return Err(VMError::StackOverflow);
        }
        self.stack.insert(self.stack_ptr, ir);
        self.stack_ptr += 1;
        Ok(())
    }

    fn push_index(&mut self, stack_index: usize) -> Result<(), VMError> {
        if self.stack_ptr >= STACK_SIZE {
            return Err(VMError::StackOverflow);
        }
        match self.bytecode.constants.get(stack_index) {
            Some(constant) => {
                self.stack.insert(self.stack_ptr, constant.clone());
                self.stack_ptr += 1;
            }
            None => panic!("Out of bounds: no constant found at index {}", stack_index),
        };
        Ok(())
    }

    fn pop(&mut self) -> IR {
        let ir = self.stack.remove(self.stack_ptr - 1);
        self.stack_ptr -= 1;
        ir
    }

    pub fn stack_top(&self) -> Option<&IR> {
        match self.stack_ptr {
            0 => None,
            n => self.stack.get(n - 1),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::Program,
        compiler::{Bytecode, Compiler},
        ir::IR,
        lexer::Lexer,
        parser::Parser,
    };
    use std::{cell::RefCell, rc::Rc};
    use string_interner::StringInterner;

    use super::*;

    struct VMTestCase<'input> {
        input: &'input str,
        expected: IR,
    }

    impl<'input> VMTestCase<'input> {
        fn new(input: &'input str, expected: IR) -> Self {
            Self { input, expected }
        }

        fn parse(&self) -> Program {
            let interner = Rc::new(RefCell::new(StringInterner::default()));
            let lexer = Lexer::new(self.input, Rc::clone(&interner));
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            for error in &program.errors {
                eprintln!("{}", error);
            }
            program
        }

        fn compile(&self) -> Bytecode {
            let program = self.parse();
            let compiler = Compiler::new();
            assert!(compiler.compile(&program).is_ok());
            compiler.to_bytecode()
        }
    }

    fn test_expected_object(expected: IR, actual: &IR) {
        match (expected, actual) {
            (IR::Integer(expected_value), IR::Integer(actual_value)) => {
                assert_eq!(expected_value, *actual_value)
            }
            (expected_ir, actual_ir) => todo!(
                "Unexpected comparison between {} and {}",
                expected_ir,
                actual_ir
            ),
        }
    }

    fn run_vm_tests(tests: Vec<VMTestCase>) {
        for test in tests {
            let bytecode = test.compile();
            let mut vm = VM::new(bytecode);
            assert!(vm.run().is_ok());
            match vm.stack_top() {
                Some(element) => test_expected_object(test.expected, element),
                None => {} //panic!("No elements found in stack"),
            }
        }
    }

    #[test]
    fn it_evaluates_integer_arithmetic() {
        let tests = vec![
            // VMTestCase::new("1", IR::Integer(1)),
            // VMTestCase::new("2", IR::Integer(2)),
            VMTestCase::new("1 + 2", IR::Integer(3)),
        ];
        run_vm_tests(tests);
    }
}
