pub mod symbol_table;

use std::{cell::RefCell, convert::TryFrom, fmt, rc::Rc};

use string_interner::StringInterner;

use crate::{
    ast::{
        Expression, Identifier, IfExpression, InfixExpression, LetStatement, PrefixExpression,
        Program, Statement,
    },
    code::{self, Byte, Instructions, Opcode},
    compiler::symbol_table::Symbol,
    ir::IR,
    token::Token,
};

use self::symbol_table::SymbolTable;

#[derive(Debug)]
pub enum CompilerError {
    NotImplementedYet(String),
    UndefinedVariable(String),
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<IR>,
}

impl fmt::Debug for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Ok(disassembled) = code::disasemble(&self.instructions) {
            write!(f, "{}", disassembled)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EmittedInstruction {
    pub opcode: Opcode,
    pub position: usize,
}

impl EmittedInstruction {
    fn new(opcode: Opcode, position: usize) -> Self {
        Self { opcode, position }
    }
}

pub struct CompilerState {
    pub interner: Rc<RefCell<StringInterner>>,
    pub constants: Rc<RefCell<Vec<IR>>>,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
}

impl CompilerState {
    pub fn new(
        interner: Rc<RefCell<StringInterner>>,
        constants: Option<Rc<RefCell<Vec<IR>>>>,
        symbol_table: Option<Rc<RefCell<SymbolTable>>>,
    ) -> Self {
        Self {
            interner,
            constants: constants.unwrap_or_default(),
            symbol_table: symbol_table.unwrap_or_default(),
        }
    }
}

pub struct Compiler {
    instructions: Rc<RefCell<Instructions>>,
    constants: Rc<RefCell<Vec<IR>>>,
    last_instr: Option<EmittedInstruction>,
    prev_instr: Option<EmittedInstruction>,
    interner: Rc<RefCell<StringInterner>>,
    symbol_table: Rc<RefCell<SymbolTable>>,
}

impl Compiler {
    pub fn new(options: CompilerState) -> Self {
        Self {
            instructions: Default::default(),
            constants: options.constants,
            last_instr: Default::default(),
            prev_instr: Default::default(),
            interner: Rc::clone(&options.interner),
            symbol_table: Rc::clone(&options.symbol_table),
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<(), CompilerError> {
        program
            .statements
            .iter()
            .try_for_each(|statement| self.compile_statement(statement))
    }

    fn compile_statement(&mut self, statement: &Statement) -> Result<(), CompilerError> {
        match statement {
            Statement::Expression(expression) => {
                self.compile_expression(expression)?;
                self.emit(Opcode::OpPop, None);
                Ok(())
            }
            Statement::Block(statements) => statements
                .iter()
                .try_for_each(|statement| self.compile_statement(statement)),
            Statement::Let(LetStatement {
                ident: Identifier(name),
                expr,
            }) => {
                self.compile_expression(expr)?;
                let symbol = self.symbol_table.borrow_mut().define(*name);
                self.emit(Opcode::OpSetGlobal, Some(&[symbol.index]));
                Ok(())
            }
            statement => Err(CompilerError::NotImplementedYet(format!(
                "{:#?}",
                statement
            ))),
        }
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<(), CompilerError> {
        match expression {
            Expression::Infix(InfixExpression { op, left, right }) => {
                match op {
                    // SPECIAL CASE
                    // We can always reorder a < b expressions into b > a, so we don't need an
                    // OpLesserThan instruction. Note that right is compiled before left.
                    Token::LessThan => {
                        self.compile_expression(right)?;
                        self.compile_expression(left)?;
                        self.emit(Opcode::OpGreaterThan, None);
                    }
                    operator => {
                        self.compile_expression(left)?;
                        self.compile_expression(right)?;
                        match operator {
                            Token::Plus => self.emit(Opcode::OpAdd, None),
                            Token::Minus => self.emit(Opcode::OpSub, None),
                            Token::Asterisk => self.emit(Opcode::OpMul, None),
                            Token::Slash => self.emit(Opcode::OpDiv, None),
                            Token::GreaterThan => self.emit(Opcode::OpGreaterThan, None),
                            Token::Equal => self.emit(Opcode::OpEqual, None),
                            Token::NotEqual => self.emit(Opcode::OpNotEqual, None),
                            operator => {
                                return Err(CompilerError::NotImplementedYet(format!(
                                    "{:#?}",
                                    operator
                                )))
                            }
                        };
                    }
                };
                Ok(())
            }
            Expression::Prefix(PrefixExpression { op, right }) => {
                self.compile_expression(right)?;
                match op {
                    Token::Bang => self.emit(Opcode::OpBang, None),
                    Token::Minus => self.emit(Opcode::OpMinus, None),
                    op => return Err(CompilerError::NotImplementedYet(format!("{:#?}", op))),
                };
                Ok(())
            }
            Expression::Integer(value) => {
                self.emit(
                    Opcode::OpConstant,
                    Some(&[self.add_constant(IR::Integer(*value))]),
                );
                Ok(())
            }
            Expression::Boolean(value) => {
                match value {
                    true => self.emit(Opcode::OpTrue, None),
                    false => self.emit(Opcode::OpFalse, None),
                };
                Ok(())
            }
            Expression::If(IfExpression {
                condition,
                consequent,
                alternate,
            }) => {
                self.compile_expression(condition)?;
                let jump_not_truthy_pos = self.emit(Opcode::OpJumpNotTruthy, Some(&[9999]));
                if let Some(consequent) = consequent {
                    self.compile_statement(&*consequent)?;
                }
                if self.last_instr_is_pop() {
                    self.remove_last_pop();
                }
                let jump_pos = self.emit(Opcode::OpJump, Some(&[9999]));
                self.change_operand(jump_not_truthy_pos, self.instructions_len());

                if let Some(alternate) = alternate {
                    self.compile_statement(&*alternate)?;
                    if self.last_instr_is_pop() {
                        self.remove_last_pop();
                    }
                } else {
                    self.emit(Opcode::OpNull, None);
                }
                self.change_operand(jump_pos, self.instructions_len());
                Ok(())
            }
            Expression::Identifier(Identifier(name)) => {
                let identifier;
                {
                    identifier = self.symbol_table.borrow_mut().resolve(*name);
                }
                match identifier {
                    Some(Symbol { index, .. }) => self.emit(Opcode::OpGetGlobal, Some(&[index])),
                    None => match self.interner.borrow().resolve(*name) {
                        Some(name) => {
                            return Err(CompilerError::UndefinedVariable(name.to_string()))
                        }
                        None => {
                            return Err(CompilerError::UndefinedVariable("unknown".to_string()))
                        }
                    },
                };
                Ok(())
            }
            expression => Err(CompilerError::NotImplementedYet(format!(
                "{:#?}",
                expression
            ))),
        }
    }

    fn emit(&mut self, opcode: Opcode, operands: Option<&[usize]>) -> usize {
        let mut instruction = code::make(opcode, operands);
        let position = self.add_instruction(&mut instruction);
        self.set_last_instr(opcode, position);
        position
    }

    fn set_last_instr(&mut self, opcode: Opcode, position: usize) {
        let prev = self.last_instr;
        let last = EmittedInstruction::new(opcode, position);
        self.prev_instr = prev;
        self.last_instr = Some(last);
    }

    fn last_instr_is_pop(&self) -> bool {
        if let Some(EmittedInstruction { opcode, .. }) = self.last_instr {
            opcode == Opcode::OpPop
        } else {
            false
        }
    }

    fn remove_last_pop(&mut self) {
        if let Some(EmittedInstruction { position, .. }) = self.last_instr {
            let mut instructions = self.instructions.borrow_mut();
            instructions.remove(position);
            self.last_instr = self.prev_instr
        }
    }

    fn add_constant(&self, ir: IR) -> usize {
        let mut constants = self.constants.borrow_mut();
        constants.push(ir);
        constants.len() - 1
    }

    fn add_instruction(&self, instruction: &mut Instructions) -> usize {
        let mut instructions = self.instructions.borrow_mut();
        let offset = instructions.len();
        instructions.append(instruction);
        offset
    }

    fn replace_instruction(&self, position: usize, new_instr: &[Byte]) {
        let mut instructions = self.instructions.borrow_mut();
        new_instr
            .iter()
            .enumerate()
            .for_each(|(i, instr)| instructions[position + i] = *instr)
    }

    fn instructions_len(&self) -> usize {
        self.instructions.borrow().len()
    }

    fn change_operand(&self, op_pos: usize, operand: usize) {
        let instructions = self.instructions.borrow_mut();
        let op = Opcode::try_from(instructions[op_pos]).unwrap();
        drop(instructions);
        self.replace_instruction(op_pos, &code::make(op, Some(&[operand])));
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
        code::{disasemble, make, Byte, Opcode},
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
        interner: Option<Rc<RefCell<StringInterner>>>,
    }

    impl<'input> CompilerTestCase<'input> {
        fn new(
            input: &'input str,
            expected_constants: Vec<IR>,
            expected_instructions: Vec<Vec<Byte>>,
        ) -> Self {
            let interner = Rc::new(RefCell::new(StringInterner::default()));
            Self {
                input,
                expected_constants,
                expected_instructions,
                interner: Some(interner),
            }
        }

        fn parse(&self) -> Program {
            let lexer = Lexer::new(self.input, Rc::clone(self.interner.as_ref().unwrap()));
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            for error in &program.errors {
                eprintln!("{}", error);
            }
            program
        }

        fn compile(&self) -> Bytecode {
            let program = self.parse();
            let compiler_options =
                CompilerState::new(Rc::clone(self.interner.as_ref().unwrap()), None, None);
            let mut compiler = Compiler::new(compiler_options);
            match compiler.compile(&program) {
                Ok(_) => compiler.to_bytecode(),
                Err(error) => panic!("{:#?}", error),
            }
        }
    }

    fn test_instructions(expected: Vec<Vec<Byte>>, actual: Instructions) {
        let concatted = expected.into_iter().flatten().collect::<Vec<_>>();
        let disasembled_expected = disasemble(&concatted).unwrap();
        let disasembled_actual = disasemble(&actual).unwrap();
        assert_eq!(
            actual.len(),
            concatted.len(),
            "Wrong instructions length.\nExpected: {:?}\n{}\n\nActual: {:?}\n{}",
            concatted,
            disasembled_expected,
            actual,
            disasembled_actual
        );
        for (index, instruction) in concatted.iter().enumerate() {
            assert_eq!(
                actual[index], *instruction,
                "Wrong instruction at {}.\nExpected: \n{}\n\nActual:\n{}",
                index, disasembled_expected, disasembled_actual
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
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpConstant, Some(&[1])),
                    make(Opcode::OpAdd, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "1; 2",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpPop, None),
                    make(Opcode::OpConstant, Some(&[1])),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "1 - 2",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpConstant, Some(&[1])),
                    make(Opcode::OpSub, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "1 * 2",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpConstant, Some(&[1])),
                    make(Opcode::OpMul, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "1 / 2",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpConstant, Some(&[1])),
                    make(Opcode::OpDiv, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "-1",
                vec![IR::Integer(1)],
                vec![
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpMinus, None),
                    make(Opcode::OpPop, None),
                ],
            ),
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn it_evaluates_boolean_expressions() {
        let tests = vec![
            CompilerTestCase::new(
                "true",
                vec![],
                vec![make(Opcode::OpTrue, None), make(Opcode::OpPop, None)],
            ),
            CompilerTestCase::new(
                "false",
                vec![],
                vec![make(Opcode::OpFalse, None), make(Opcode::OpPop, None)],
            ),
            CompilerTestCase::new(
                "1 > 2",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpConstant, Some(&[1])),
                    make(Opcode::OpGreaterThan, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "1 < 2",
                vec![IR::Integer(2), IR::Integer(1)],
                vec![
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpConstant, Some(&[1])),
                    make(Opcode::OpGreaterThan, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "1 == 2",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpConstant, Some(&[1])),
                    make(Opcode::OpEqual, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "1 != 2",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpConstant, Some(&[1])),
                    make(Opcode::OpNotEqual, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "true == false",
                vec![],
                vec![
                    make(Opcode::OpTrue, None),
                    make(Opcode::OpFalse, None),
                    make(Opcode::OpEqual, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "true != false",
                vec![],
                vec![
                    make(Opcode::OpTrue, None),
                    make(Opcode::OpFalse, None),
                    make(Opcode::OpNotEqual, None),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "!true",
                vec![],
                vec![
                    make(Opcode::OpTrue, None),
                    make(Opcode::OpBang, None),
                    make(Opcode::OpPop, None),
                ],
            ),
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn it_compiles_conditionals() {
        let tests = vec![
            CompilerTestCase::new(
                "if (true) { 10 }; 3333;",
                vec![IR::Integer(10), IR::Integer(3333)],
                vec![
                    make(Opcode::OpTrue, None),                 // 0000
                    make(Opcode::OpJumpNotTruthy, Some(&[10])), // 0001
                    make(Opcode::OpConstant, Some(&[0])),       // 0004
                    make(Opcode::OpJump, Some(&[11])),          // 0007
                    make(Opcode::OpNull, None),                 // 0010
                    make(Opcode::OpPop, None),                  // 0011
                    make(Opcode::OpConstant, Some(&[1])),       // 0012
                    make(Opcode::OpPop, None),                  // 0015
                ],
            ),
            CompilerTestCase::new(
                "if (true) { 10 } else { 20 }; 3333;",
                vec![IR::Integer(10), IR::Integer(20), IR::Integer(3333)],
                vec![
                    make(Opcode::OpTrue, None),                 // 0000
                    make(Opcode::OpJumpNotTruthy, Some(&[10])), // 0001
                    make(Opcode::OpConstant, Some(&[0])),       // 0004
                    make(Opcode::OpJump, Some(&[13])),          // 0007
                    make(Opcode::OpConstant, Some(&[1])),       // 0010
                    make(Opcode::OpPop, None),                  // 0013
                    make(Opcode::OpConstant, Some(&[2])),       // 0014
                    make(Opcode::OpPop, None),                  // 0017
                ],
            ),
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn it_compiles_let_statements() {
        let tests = vec![
            CompilerTestCase::new(
                "let one = 1; let two = 2;",
                vec![IR::Integer(1), IR::Integer(2)],
                vec![
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpSetGlobal, Some(&[0])),
                    make(Opcode::OpConstant, Some(&[1])),
                    make(Opcode::OpSetGlobal, Some(&[1])),
                ],
            ),
            CompilerTestCase::new(
                "let one = 1; one;",
                vec![IR::Integer(1)],
                vec![
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpSetGlobal, Some(&[0])),
                    make(Opcode::OpGetGlobal, Some(&[0])),
                    make(Opcode::OpPop, None),
                ],
            ),
            CompilerTestCase::new(
                "let one = 1; let two = one; two;",
                vec![IR::Integer(1)],
                vec![
                    make(Opcode::OpConstant, Some(&[0])),
                    make(Opcode::OpSetGlobal, Some(&[0])),
                    make(Opcode::OpGetGlobal, Some(&[0])),
                    make(Opcode::OpSetGlobal, Some(&[1])),
                    make(Opcode::OpGetGlobal, Some(&[1])),
                    make(Opcode::OpPop, None),
                ],
            ),
        ];
        run_compiler_tests(tests);
    }
}
