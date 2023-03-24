use std::fmt;

use string_interner::symbol::SymbolU32;

use crate::{
    ast::Identifier,
    token::{self, Token},
};

pub type InstructionId = u32;

#[derive(Clone, Debug)]
pub enum InstructionValue {
    Identifier(SymbolU32),
    Integer(token::IntegerSize),
    String(SymbolU32),
    Boolean(bool),

    Infix(Box<InfixInstructionValue>),
}

#[derive(Clone, Debug)]
pub struct InfixInstructionValue {
    pub(crate) operator: Token,
    pub(crate) left: InstructionValue,
    pub(crate) right: InstructionValue,
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Const(ConstInstruction),
    Terminal(Terminal),
}

#[derive(Clone, Debug)]
pub struct ConstInstruction {
    pub(crate) id: InstructionId,
    pub(crate) lvalue: Identifier,
    pub(crate) value: InstructionValue,
}

#[derive(Clone, Debug)]
pub enum Terminal {
    Return(ReturnTerminal),
}

#[derive(Clone, Debug)]
pub struct ReturnTerminal {
    pub(crate) value: InstructionValue,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Const(instr) => {
                write!(f, "[{}] Const {} = {}", instr.id, instr.lvalue, instr.value)
            }
            Instruction::Terminal(terminal) => terminal.fmt(f),
        }
    }
}

impl fmt::Display for InstructionValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InstructionValue::Integer(int) => int.fmt(f),
            InstructionValue::Identifier(ident) => write!(f, "Identifier({:?})", ident),
            InstructionValue::String(str) => write!(f, "String({:?})", str),
            InstructionValue::Boolean(bool) => bool.fmt(f),
            InstructionValue::Infix(infix) => {
                write!(f, "{} {} {}", infix.left, infix.operator, infix.right)
            }
        }
    }
}

impl fmt::Display for Terminal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Terminal::Return(terminal) => write!(f, "Return {}", terminal.value),
        }
    }
}
