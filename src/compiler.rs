use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Expression, Program, Statement},
    code::{self, Instructions, Opcode},
    ir::IR,
    token::Token,
};

#[derive(Debug)]
pub enum CompilerError {
    NotImplementedYet,
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<IR>,
}

pub struct Compiler {
    instructions: Rc<RefCell<Instructions>>,
    constants: Rc<RefCell<Vec<IR>>>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Default::default(),
            constants: Default::default(),
        }
    }

    pub fn compile(&self, program: &Program) -> Result<(), CompilerError> {
        program
            .statements
            .iter()
            .map(|statement| self.compile_statement(statement))
            .collect()
    }

    fn compile_statement(&self, statement: &Statement) -> Result<(), CompilerError> {
        match statement {
            Statement::Expression(expression) => {
                self.compile_expression(expression)?;
                self.emit(Opcode::OpPop, None);
                Ok(())
            }
            _ => Err(CompilerError::NotImplementedYet),
        }
    }

    fn compile_expression(&self, expression: &Expression) -> Result<(), CompilerError> {
        match expression {
            Expression::Infix(operator, left, right) => {
                self.compile_expression(left)?;
                self.compile_expression(right)?;
                match operator {
                    Token::Plus => self.emit(Opcode::OpAdd, None),
                    Token::Minus => self.emit(Opcode::OpSub, None),
                    Token::Asterisk => self.emit(Opcode::OpMul, None),
                    Token::Slash => self.emit(Opcode::OpDiv, None),
                    _ => return Err(CompilerError::NotImplementedYet),
                };
                Ok(())
            }
            Expression::Integer(value) => {
                self.emit(
                    Opcode::OpConstant,
                    Some(&vec![self.add_constant(IR::Integer(*value))]),
                );
                Ok(())
            }
            _ => Err(CompilerError::NotImplementedYet),
        }
    }

    fn emit(&self, opcode: Opcode, operands: Option<&Vec<usize>>) -> usize {
        let mut instruction = code::make(opcode, operands);
        self.add_instructions(&mut instruction)
    }

    fn add_constant(&self, ir: IR) -> usize {
        let mut constants = self.constants.borrow_mut();
        constants.push(ir);
        constants.len() - 1
    }

    fn add_instructions(&self, instructions: &mut Instructions) -> usize {
        let offset = instructions.len();
        self.instructions.borrow_mut().append(instructions);
        offset
    }

    pub fn to_bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.borrow().clone(),
            constants: self.constants.borrow().clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::Program,
        code::{make, Byte, Opcode},
        lexer::Lexer,
        parser::Parser,
    };
    use std::{cell::RefCell, rc::Rc};
    use string_interner::StringInterner;

    use super::*;

    struct CompilerTestCase<'input> {
        input: &'input str,
        expected_constants: Vec<IR>,
        expected_instructions: Vec<Vec<Byte>>,
    }

    impl<'input> CompilerTestCase<'input> {
        fn new(
            input: &'input str,
            expected_constants: Vec<IR>,
            expected_instructions: Vec<Vec<Byte>>,
        ) -> Self {
            Self {
                input,
                expected_constants,
                expected_instructions,
            }
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

    fn test_instructions(expected: Vec<Vec<Byte>>, actual: Instructions) {
        let concatted = expected.into_iter().flatten().collect::<Vec<_>>();
        assert_eq!(
            actual.len(),
            concatted.len(),
            "Wrong instructions length. Expected: {:?}, got: {:?}",
            concatted,
            actual
        );
        for (index, instruction) in concatted.iter().enumerate() {
            assert_eq!(
                actual[index], *instruction,
                "Wrong instruction at {}. Expected: {:?}, got: {:?}",
                index, concatted, actual
            );
        }
    }

    fn test_constants(expected: Vec<IR>, actual: Vec<IR>) {
        assert_eq!(
            expected.len(),
            actual.len(),
            "Wrong constants length. Expected: {:?}, got: {:?}",
            expected,
            actual
        );
        for (index, constant) in expected.iter().enumerate() {
            match (constant, &actual[index]) {
                (IR::Integer(expected_value), IR::Integer(actual_value)) => {
                    assert_eq!(expected_value, actual_value)
                }
                (left, right) => panic!("Unhandled IRs. Left: {}, Right: {}", left, right),
            }
        }
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for test in tests {
            let bytecode = test.compile();
            test_instructions(test.expected_instructions, bytecode.instructions);
            test_constants(test.expected_constants, bytecode.constants);
        }
    }

    #[test]
    fn it_compiles_integer_arithmectic() {
        let tests = vec![
            CompilerTestCase::new(
                "1 + 2",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&vec![0])),
                    make(Opcode::OpConstant, Some(&vec![1])),
                    make(Opcode::OpAdd, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "1; 2",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&vec![0])),
                    make(Opcode::OpPop, None),
                    make(Opcode::OpConstant, Some(&vec![1])),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "1 - 2",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&vec![0])),
                    make(Opcode::OpConstant, Some(&vec![1])),
                    make(Opcode::OpSub, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "1 * 2",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&vec![0])),
                    make(Opcode::OpConstant, Some(&vec![1])),
                    make(Opcode::OpMul, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "1 / 2",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&vec![0])),
                    make(Opcode::OpConstant, Some(&vec![1])),
                    make(Opcode::OpDiv, None),
                    make(Opcode::OpPop, None),
                ],
            ),
        ];

        run_compiler_tests(tests);
    }
}
