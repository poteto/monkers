use crate::{
    ast::{Expression, Identifier, Program, Statement},
    token,
};
use std::fmt;

type InstructionId = u32;

#[derive(Debug)]
pub enum HIRError {
    NotImplementedYet(String),
}

pub struct ConstInstr {
    id: InstructionId,
    lvalue: Identifier,
    value: InstructionValue,
}

pub enum InstructionValue {
    Integer(token::IntegerSize),
}

pub enum Instruction {
    Const(ConstInstr),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Const(instr) => {
                write!(f, "[{}] Const {} = {}", instr.id, instr.lvalue, instr.value)
            }
        }
    }
}

impl fmt::Display for InstructionValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InstructionValue::Integer(int) => write!(f, "{}", int),
        }
    }
}

pub struct HIR {
    entry: BasicBlock,
}

pub struct BasicBlock {
    instructions: Vec<Instruction>,
    preds: Vec<BasicBlock>,
    succs: Vec<BasicBlock>,
}

//
pub struct HIRBuilder {
    hir: Option<HIR>,
    curr_id: InstructionId,
    pub wip_instrs: Vec<Instruction>,
}

impl HIRBuilder {
    pub fn new() -> Self {
        Self {
            hir: Default::default(),
            curr_id: 0,
            wip_instrs: Default::default(),
        }
    }

    pub fn lower(&mut self, program: &Program) -> Result<(), HIRError> {
        program
            .statements
            .iter()
            .try_for_each(|statement| self.lower_statement(statement))
    }

    fn lower_statement(&mut self, statement: &Statement) -> Result<(), HIRError> {
        match statement {
            Statement::Expression(_) => todo!(),
            Statement::Let(ident, expr) => {
                let instr = Instruction::Const(ConstInstr {
                    lvalue: ident.clone(),
                    id: self.next_instr_id(),
                    value: self.lower_expression(&expr)?,
                });
                self.wip_instrs.push(instr);
            }
            Statement::Return(_) => todo!(),
            Statement::Block(_) => todo!(),
        }
        self.curr_id += 1;
        Ok(())
    }

    fn lower_expression(&self, expression: &Expression) -> Result<InstructionValue, HIRError> {
        match expression {
            Expression::Identifier(_) => todo!(),
            Expression::Integer(int) => Ok(InstructionValue::Integer(*int)),
            Expression::String(_) => todo!(),
            Expression::Boolean(_) => todo!(),
            Expression::Prefix(_, _) => todo!(),
            Expression::Infix(_, _, _) => todo!(),
            Expression::Index(_, _) => todo!(),
            Expression::If(_, _, _) => todo!(),
            Expression::Function(_, _) => todo!(),
            Expression::Call(_, _) => todo!(),
            Expression::Array(_) => todo!(),
            Expression::Hash(_) => todo!(),
        }
    }

    fn next_instr_id(&mut self) -> u32 {
        let ret = self.curr_id;
        self.curr_id += 1;
        ret
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Program;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use std::{cell::RefCell, rc::Rc};
    use string_interner::StringInterner;

    use super::HIRBuilder;

    fn parse(input: &str) -> Program {
        let interner = Rc::new(RefCell::new(StringInterner::default()));
        let lexer = Lexer::new(input, Rc::clone(&interner));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        for error in &program.errors {
            eprintln!("{}", error);
        }

        program
    }

    #[test]
    fn it_builds_an_hir() {
        let tests = vec![("let x = 5;", "[0] Const Identifier(0) = 5")];

        for (input, expected) in tests {
            let program = parse(input);
            let mut builder = HIRBuilder::new();
            builder.lower(&program).expect("yes");

            // todo: it'd be much nicer to assert the entire cfg rather than statement by statement
            //       also don't expose wip_instrs
            for instr in &builder.wip_instrs {
                assert_eq!(expected, instr.to_string());
            }
        }
    }
}
