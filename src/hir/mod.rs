mod instruction;

use crate::ast::{Expression, Identifier, LetStatement, Program, Statement};
use fnv::FnvHashMap;
use instruction::{
    ConstInstruction, Instruction, InstructionId, InstructionValue, ReturnTerminal, Terminal,
};

use self::instruction::InfixInstructionValue;

type BlockId = u32;

#[derive(Debug)]
pub enum HIRError {
    NotImplementedYet(String),
}

#[derive(Debug)]
pub struct HIR {
    entry: BlockId,
    blocks: FnvHashMap<BlockId, BasicBlock>,
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
    curr_instr_id: InstructionId,
    curr_block_id: BlockId,
    wip_instrs: Vec<Instruction>,
}

impl HIRBuilder {
    pub fn new() -> Self {
        Self {
            hir: Default::default(),
            curr_instr_id: 0,
            curr_block_id: 0,
            wip_instrs: Default::default(),
        }
    }

    pub fn lower(&mut self, program: &Program) -> Result<HIR, HIRError> {
        program
            .statements
            .iter()
            .try_for_each(|statement| self.lower_statement(statement))?;

        Ok(HIR {
            entry: 0, // todo: figure out entry block
            blocks: self.form_basic_blocks(),
        })
    }

    fn form_basic_blocks(&mut self) -> FnvHashMap<BlockId, BasicBlock> {
        let mut blocks: FnvHashMap<BlockId, BasicBlock> = FnvHashMap::default();
        let mut wip_block = BasicBlock::new(self.curr_block_id);

        for instr in &self.wip_instrs {
            wip_block.push_instr(instr.clone());
            if let Instruction::Terminal(_) = instr {
                blocks.insert(self.curr_block_id, wip_block);
                self.curr_block_id += 1;
                wip_block = BasicBlock::new(self.curr_block_id)
            }
        }

        blocks
    }

    fn lower_statement(&mut self, statement: &Statement) -> Result<(), HIRError> {
        match statement {
            Statement::Expression(_) => todo!(),
            Statement::Let(LetStatement { ident, expr }) => {
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
        self.curr_instr_id += 1;
        Ok(())
    }

    fn lower_expression(&self, expression: &Expression) -> Result<InstructionValue, HIRError> {
        match expression {
            Expression::Identifier(Identifier(name)) => Ok(InstructionValue::Identifier(*name)),
            Expression::Integer(int) => Ok(InstructionValue::Integer(*int)),
            Expression::String(str) => Ok(InstructionValue::String(*str)),
            Expression::Boolean(bool) => Ok(InstructionValue::Boolean(*bool)),
            Expression::Prefix(_, _) => todo!(),
            Expression::Infix(op, left, right) => {
                let left = self.lower_expression(left)?;
                let right = self.lower_expression(right)?;
                Ok(InstructionValue::Infix(Box::new(InfixInstructionValue {
                    operator: op.clone(),
                    left,
                    right,
                })))
            }
            Expression::Index(_, _) => todo!(),
            Expression::If(_, _, _) => todo!(),
            Expression::Function(_, _) => todo!(),
            Expression::Call(_, _) => todo!(),
            Expression::Array(_) => todo!(),
            Expression::Hash(_) => todo!(),
        }
    }

    fn next_instr_id(&mut self) -> u32 {
        let ret = self.curr_instr_id;
        self.curr_instr_id += 1;
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
        // todo: currently nonsensical input but demonstrates that we can handle multiple blocks
        let program = parse("let x = 5 + 5; let y = x + 5; return x + y; return x;");
        let mut builder = HIRBuilder::new();
        let hir = builder.lower(&program).expect("lowers to HIR");

        let expected = expect![[r#"
            HIR {
                entry: 0,
                blocks: {
                    1: BasicBlock {
                        id: 1,
                        instructions: [
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
                    0: BasicBlock {
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
                                    value: Infix(
                                        InfixInstructionValue {
                                            operator: Plus,
                                            left: Integer(
                                                5,
                                            ),
                                            right: Integer(
                                                5,
                                            ),
                                        },
                                    ),
                                },
                            ),
                            Const(
                                ConstInstruction {
                                    id: 2,
                                    lvalue: Identifier(
                                        SymbolU32 {
                                            value: 2,
                                        },
                                    ),
                                    value: Infix(
                                        InfixInstructionValue {
                                            operator: Plus,
                                            left: Identifier(
                                                SymbolU32 {
                                                    value: 1,
                                                },
                                            ),
                                            right: Integer(
                                                5,
                                            ),
                                        },
                                    ),
                                },
                            ),
                            Terminal(
                                Return(
                                    ReturnTerminal {
                                        value: Infix(
                                            InfixInstructionValue {
                                                operator: Plus,
                                                left: Identifier(
                                                    SymbolU32 {
                                                        value: 1,
                                                    },
                                                ),
                                                right: Identifier(
                                                    SymbolU32 {
                                                        value: 2,
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                ),
                            ),
                        ],
                        preds: [],
                        succs: [],
                    },
                },
            }
        "#]];
        expected.assert_debug_eq(&hir);
    }
}
