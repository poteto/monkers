use std::convert::TryFrom;

use crate::{
    code::{self, Opcode},
    compiler::Bytecode,
    ir::IR,
};

const STACK_SIZE: usize = 2048;
const TRUE: IR = IR::Boolean(true);
const FALSE: IR = IR::Boolean(false);
const NULL: IR = IR::Null;

#[derive(Debug)]
pub enum VMError {
    NotImplementedYet,
    StackOverflow,
    UnknownOperator(Opcode),
    TypeError(IR),
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
                Ok(opcode) => self.execute_instruction(opcode, &mut instruction_ptr)?,
                _ => return Err(VMError::NotImplementedYet),
            };
            instruction_ptr += 1;
        }
        Ok(())
    }

    fn execute_instruction(
        &mut self,
        opcode: Opcode,
        instruction_ptr: &mut usize,
    ) -> Result<(), VMError> {
        match opcode {
            Opcode::OpConstant => {
                let const_index =
                    code::read_u16(&self.bytecode.instructions[(*instruction_ptr + 1)..]);
                *instruction_ptr += 2;
                self.push_index(const_index.into())?;
                Ok(())
            }
            Opcode::OpAdd => self.execute_binary_operation(Opcode::OpAdd),
            Opcode::OpSub => self.execute_binary_operation(Opcode::OpSub),
            Opcode::OpMul => self.execute_binary_operation(Opcode::OpMul),
            Opcode::OpDiv => self.execute_binary_operation(Opcode::OpDiv),
            Opcode::OpPop => {
                self.pop();
                Ok(())
            }
            Opcode::OpTrue => self.push(TRUE),
            Opcode::OpFalse => self.push(FALSE),
            Opcode::OpEqual | Opcode::OpNotEqual | Opcode::OpGreaterThan => {
                self.execute_comparison(opcode)
            }
            Opcode::OpMinus => self.execute_minus_operator(),
            Opcode::OpBang => self.execute_bang_operator(),
            Opcode::OpJump => {
                let pos = code::read_u16(&self.bytecode.instructions[(*instruction_ptr + 1)..]);
                *instruction_ptr = (pos - 1).into();
                Ok(())
            }
            Opcode::OpJumpNotTruthy => {
                let pos = code::read_u16(&self.bytecode.instructions[(*instruction_ptr + 1)..]);
                *instruction_ptr += 2;
                let condition = self.pop();
                if !self.is_truthy(condition) {
                    *instruction_ptr = (pos - 1).into();
                }
                Ok(())
            }
            Opcode::OpNull => self.push(NULL),
        }
    }

    fn execute_binary_operation(&mut self, opcode: Opcode) -> Result<(), VMError> {
        let right = self.pop();
        let left = self.pop();
        match (left, right) {
            (IR::Integer(left_value), IR::Integer(right_value)) => match opcode {
                Opcode::OpAdd => self.push(IR::Integer(left_value + right_value)),
                Opcode::OpSub => self.push(IR::Integer(left_value - right_value)),
                Opcode::OpMul => self.push(IR::Integer(left_value * right_value)),
                Opcode::OpDiv => self.push(IR::Integer(left_value / right_value)),
                opcode => Err(VMError::UnknownOperator(opcode)),
            },
            _ => Err(VMError::NotImplementedYet),
        }
    }

    fn execute_comparison(&mut self, opcode: Opcode) -> Result<(), VMError> {
        let right = self.pop();
        let left = self.pop();
        match (left, right) {
            (IR::Integer(left_value), IR::Integer(right_value)) => match opcode {
                Opcode::OpEqual => self.push(self.get_interned_bool(&(right_value == left_value))),
                Opcode::OpNotEqual => {
                    self.push(self.get_interned_bool(&(right_value != left_value)))
                }
                Opcode::OpGreaterThan => {
                    self.push(self.get_interned_bool(&(left_value > right_value)))
                }
                opcode => Err(VMError::UnknownOperator(opcode)),
            },
            (left, right) => match opcode {
                Opcode::OpEqual => self.push(self.get_interned_bool(&(right == left))),
                Opcode::OpNotEqual => self.push(self.get_interned_bool(&(right != left))),
                opcode => Err(VMError::UnknownOperator(opcode)),
            },
        }
    }

    fn execute_minus_operator(&mut self) -> Result<(), VMError> {
        match self.pop() {
            IR::Integer(value) => self.push(IR::Integer(-value)),
            ir => Err(VMError::TypeError(ir)),
        }
    }

    fn execute_bang_operator(&mut self) -> Result<(), VMError> {
        match self.pop() {
            IR::Boolean(true) => self.push(FALSE),
            IR::Boolean(false) => self.push(TRUE),
            IR::Null => self.push(TRUE),
            _ => self.push(FALSE),
        }
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

    /// VM::pop decrements the stack_ptr but does not remove the element from the stack. We return
    /// a clone here as the popped element may be overwritten at a later point.
    fn pop(&mut self) -> IR {
        let ir = self
            .stack
            .get(self.stack_ptr - 1)
            .expect("Tried to pop out of bounds index");
        self.stack_ptr -= 1;
        ir.clone()
    }

    pub fn stack_top(&self) -> Option<&IR> {
        match self.stack_ptr {
            0 => None,
            n => self.stack.get(n - 1),
        }
    }

    pub fn last_popped_stack_element(&self) -> IR {
        self.stack
            .get(self.stack_ptr)
            .expect("Tried to pop out of bounds index")
            .clone()
    }

    fn get_interned_bool(&self, native_value: &bool) -> IR {
        match native_value {
            true => TRUE,
            false => FALSE,
        }
    }

    fn is_truthy(&self, ir: IR) -> bool {
        match ir {
            IR::Boolean(value) => value,
            IR::Null => false,
            _ => true,
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
            let mut compiler = Compiler::default();
            assert!(compiler.compile(&program).is_ok());
            compiler.to_bytecode()
        }
    }

    fn test_expected_object(expected: &IR, actual: IR) {
        match (expected, actual) {
            (IR::Integer(expected_value), IR::Integer(actual_value)) => {
                assert_eq!(expected_value, &actual_value);
            }
            (IR::Boolean(expected_value), IR::Boolean(actual_value)) => {
                assert_eq!(expected_value, &actual_value);
            }
            (IR::Null, actual) => {
                assert_eq!(IR::Null, actual);
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
            match vm.run() {
                Ok(_) => test_expected_object(&test.expected, vm.last_popped_stack_element()),
                Err(error) => panic!("{:#?}", error),
            }
        }
    }

    #[test]
    fn it_evaluates_integer_arithmetic() {
        let tests = vec![
            VMTestCase::new("1", IR::Integer(1)),
            VMTestCase::new("2", IR::Integer(2)),
            VMTestCase::new("1 + 2", IR::Integer(3)),
            VMTestCase::new("1 - 2", IR::Integer(-1)),
            VMTestCase::new("1 * 2", IR::Integer(2)),
            VMTestCase::new("4 / 2", IR::Integer(2)),
            VMTestCase::new("50 / 2 * 2 + 10 - 5", IR::Integer(55)),
            VMTestCase::new("5 + 5 + 5 + 5 - 10", IR::Integer(10)),
            VMTestCase::new("2 * 2 * 2 * 2 * 2", IR::Integer(32)),
            VMTestCase::new("5 * 2 + 10", IR::Integer(20)),
            VMTestCase::new("5 + 2 * 10", IR::Integer(25)),
            VMTestCase::new("5 * (2 + 10)", IR::Integer(60)),
            VMTestCase::new("-5", IR::Integer(-5)),
            VMTestCase::new("-10", IR::Integer(-10)),
            VMTestCase::new("-50 + 100 + -50", IR::Integer(0)),
            VMTestCase::new("(5 + 10 * 2 + 15 / 3) * 2 + -10", IR::Integer(50)),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn it_evaluates_boolean_expressions() {
        let tests = vec![
            VMTestCase::new("true", IR::Boolean(true)),
            VMTestCase::new("false", IR::Boolean(false)),
            VMTestCase::new("1 < 2", IR::Boolean(true)),
            VMTestCase::new("1 > 2", IR::Boolean(false)),
            VMTestCase::new("1 < 1", IR::Boolean(false)),
            VMTestCase::new("1 > 1", IR::Boolean(false)),
            VMTestCase::new("1 == 1", IR::Boolean(true)),
            VMTestCase::new("1 != 1", IR::Boolean(false)),
            VMTestCase::new("1 == 2", IR::Boolean(false)),
            VMTestCase::new("1 != 2", IR::Boolean(true)),
            VMTestCase::new("true == true", IR::Boolean(true)),
            VMTestCase::new("false == false", IR::Boolean(true)),
            VMTestCase::new("true == false", IR::Boolean(false)),
            VMTestCase::new("true != false", IR::Boolean(true)),
            VMTestCase::new("false != true", IR::Boolean(true)),
            VMTestCase::new("(1 < 2) == true", IR::Boolean(true)),
            VMTestCase::new("(1 < 2) == false", IR::Boolean(false)),
            VMTestCase::new("(1 > 2) == true", IR::Boolean(false)),
            VMTestCase::new("(1 > 2) == false", IR::Boolean(true)),
            VMTestCase::new("!true", IR::Boolean(false)),
            VMTestCase::new("!false", IR::Boolean(true)),
            VMTestCase::new("!5", IR::Boolean(false)),
            VMTestCase::new("!!true", IR::Boolean(true)),
            VMTestCase::new("!!false", IR::Boolean(false)),
            VMTestCase::new("!!5", IR::Boolean(true)),
            VMTestCase::new("!(if (false) { 5; })", IR::Boolean(true)),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn it_evaluates_conditionals() {
        let tests = vec![
            VMTestCase::new("if (true) { 10 }", IR::Integer(10)),
            VMTestCase::new("if (true) { 10 } else { 20 }", IR::Integer(10)),
            VMTestCase::new("if (false) { 10 } else { 20 }", IR::Integer(20)),
            VMTestCase::new("if (1) { 10 }", IR::Integer(10)),
            VMTestCase::new("if (1 < 2) { 10 }", IR::Integer(10)),
            VMTestCase::new("if (1 < 2) { 10 } else { 20 }", IR::Integer(10)),
            VMTestCase::new("if (1 > 2) { 10 } else { 20 }", IR::Integer(20)),
            VMTestCase::new("if (1 > 2) { 10 }", IR::Null),
            VMTestCase::new("if (false) { 10 }", IR::Null),
            VMTestCase::new(
                "if ((if (false) { 10 })) { 10 } else { 20 }",
                IR::Integer(20),
            ),
        ];
        run_vm_tests(tests);
    }
}
