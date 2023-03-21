mod instruction;

use crate::ast::{Expression, Identifier, Program, Statement};
use instruction::{
    ConstInstruction, Instruction, InstructionId, InstructionValue, ReturnTerminal, Terminal,
};

type BlockId = u32;

#[derive(Debug)]
pub enum HIRError {
    NotImplementedYet(String),
}

#[derive(Debug)]
pub struct HIR {
    entry: BlockId,
    blocks: Vec<BasicBlock>,
}

#[derive(Debug)]
pub struct BasicBlock {
    id: BlockId,
    instructions: Vec<Instruction>,
    preds: Vec<BasicBlock>,
    succs: Vec<BasicBlock>,
}

impl BasicBlock {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            instructions: Default::default(),
            preds: Default::default(),
            succs: Default::default(),
        }
    }

    pub fn push_instr(&mut self, instr: Instruction) {
        self.instructions.push(instr)
    }
}

pub struct HIRBuilder {
    hir: Option<HIR>,
    curr_id: InstructionId,
    wip_instrs: Vec<Instruction>,
}

impl HIRBuilder {
    pub fn new() -> Self {
        Self {
            hir: Default::default(),
            curr_id: 0,
            wip_instrs: Default::default(),
        }
    }

    pub fn lower(&mut self, program: &Program) -> Result<HIR, HIRError> {
        program
            .statements
            .iter()
            .try_for_each(|statement| self.lower_statement(statement))?;

        Ok(HIR {
            entry: 0,
            blocks: self.form_basic_blocks(),
        })
    }

    fn form_basic_blocks(&self) -> Vec<BasicBlock> {
        let mut block_id = 0;
        let mut blocks: Vec<BasicBlock> = Vec::new();
        let mut wip_block = BasicBlock::new(block_id);

        for instr in &self.wip_instrs {
            wip_block.push_instr(instr.clone());
            if let Instruction::Terminal(_) = instr {
                blocks.push(wip_block);
                block_id += 1;
                wip_block = BasicBlock::new(block_id)
            }
        }

        blocks
    }

    fn lower_statement(&mut self, statement: &Statement) -> Result<(), HIRError> {
        match statement {
            Statement::Expression(_) => todo!(),
            Statement::Let(ident, expr) => {
                let instr = Instruction::Const(ConstInstruction {
                    lvalue: ident.clone(),
                    id: self.next_instr_id(),
                    value: self.lower_expression(&expr)?,
                });
                self.wip_instrs.push(instr);
            }
            Statement::Return(expr) => {
                let instr = Instruction::Terminal(Terminal::Return(ReturnTerminal {
                    value: self.lower_expression(&expr)?,
                }));
                self.wip_instrs.push(instr);
            }
            Statement::Block(_) => todo!(),
        }
        self.curr_id += 1;
        Ok(())
    }

    fn lower_expression(&self, expression: &Expression) -> Result<InstructionValue, HIRError> {
        match expression {
            Expression::Identifier(Identifier(name)) => Ok(InstructionValue::Identifier(*name)),
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
    use expect_test::expect;

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
        let tests = vec!["let x = 5; return x;"];

        for input in tests {
            let program = parse(input);
            let mut builder = HIRBuilder::new();
            let hir = builder.lower(&program).expect("yes");

            let expected = expect![[r#"
                HIR {
                    entry: 0,
                    blocks: [
                        BasicBlock {
                            id: 0,
                            instructions: [
                                Const(
                                    ConstInstruction {
                                        id: 0,
                                        lvalue: Identifier(
                                            SymbolU32 {
                                                value: 1,
                                            },
                                        ),
                                        value: Integer(
                                            5,
                                        ),
                                    },
                                ),
                                Terminal(
                                    Return(
                                        ReturnTerminal {
                                            value: Identifier(
                                                SymbolU32 {
                                                    value: 1,
                                                },
                                            ),
                                        },
                                    ),
                                ),
                            ],
                            preds: [],
                            succs: [],
                        },
                    ],
                }
            "#]];
            expected.assert_debug_eq(&hir);
        }
    }
}
