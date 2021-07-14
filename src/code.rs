use byteorder::{BigEndian, ByteOrder};
use std::fmt;

// type Instructions = [u8];

// Opcodes are represented by a single byte.
#[repr(u8)]
pub enum Opcode {
    OpConstant = 0,
}

// Definitions map Opcodes to the number of bytes each operand takes up.
enum OpcodeDefinition<'operand> {
    // Constant expressions.
    OpConstant(&'operand [usize]),
}

impl<'operand> OpcodeDefinition<'operand> {
    pub fn lookup(op: &Opcode) -> OpcodeDefinition<'operand> {
        match op {
            Opcode::OpConstant => OpcodeDefinition::OpConstant(&[2]),
        }
    }

    pub fn widths(&self) -> &[usize] {
        match self {
            OpcodeDefinition::OpConstant(widths) => widths,
        }
    }
}

impl<'opcode> fmt::Display for OpcodeDefinition<'opcode> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpcodeDefinition::OpConstant(_) => write!(f, "OpConstant"),
        }
    }
}

pub fn make(op: Opcode, operands: &[usize]) -> Vec<u8> {
    let definition = OpcodeDefinition::lookup(&op);
    let instruction_len = definition.widths().iter().fold(1, |len, w| len + w);

    let mut instruction: Vec<u8> = vec![0; instruction_len];
    instruction[0] = op as u8;

    let mut offset = 1;
    for (index, operand) in operands.iter().enumerate() {
        let width = definition.widths()[index];
        match width {
            2 => BigEndian::write_u16(&mut instruction[offset..], *operand as u16),
            _ => {}
        };
        offset += width;
    }

    instruction
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]

    fn it_makes_bytecode_instructions() {
        let tests = vec![(
            Opcode::OpConstant,
            [65534],
            [Opcode::OpConstant as u8, 255, 254],
        )];

        for (op, operands, expected) in tests {
            let instruction = make(op, &operands);

            assert_eq!(instruction, expected);
        }
    }
}
