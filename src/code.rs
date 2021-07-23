use byteorder::{BigEndian, ByteOrder};
use std::{borrow::Borrow, convert::TryFrom, fmt};

pub type Byte = u8;
pub type Instructions = Vec<Byte>;

pub enum CodeError {
    NotImplementedYet,
    UndefinedOpcode(Byte),
}

// Opcodes are represented by a single byte.
#[derive(Copy, Clone)]
#[repr(u8)]
pub enum Opcode {
    OpConstant = 0,
    OpAdd,
    OpPop,
}

// TODO: Maybe turn this into a macro?
impl TryFrom<Byte> for Opcode {
    type Error = CodeError;

    fn try_from(op: Byte) -> Result<Self, Self::Error> {
        match op {
            0 => Ok(Opcode::OpConstant),
            1 => Ok(Opcode::OpAdd),
            2 => Ok(Opcode::OpPop),
            _ => Err(CodeError::UndefinedOpcode(op)),
        }
    }
}

// Definitions map Opcodes to the number of bytes each operand takes up.
pub enum OpcodeDefinition<'operand> {
    // Constant expressions.
    OpConstant(&'operand [usize]),
    OpAdd,
    OpPop,
}

impl<'operand> OpcodeDefinition<'operand> {
    pub fn lookup(opcode: &Opcode) -> OpcodeDefinition<'operand> {
        match opcode {
            Opcode::OpConstant => OpcodeDefinition::OpConstant(&[2]),
            Opcode::OpAdd => OpcodeDefinition::OpAdd,
            Opcode::OpPop => OpcodeDefinition::OpPop,
        }
    }

    pub fn lookup_byte(byte: Byte) -> Result<OpcodeDefinition<'operand>, CodeError> {
        Ok(Self::lookup(&Opcode::try_from(byte)?))
    }

    pub fn widths(&self) -> &[usize] {
        match self {
            OpcodeDefinition::OpConstant(widths) => widths,
            OpcodeDefinition::OpAdd => &[],
            OpcodeDefinition::OpPop => &[],
        }
    }
}

impl<'opcode> fmt::Display for OpcodeDefinition<'opcode> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpcodeDefinition::OpConstant(_) => write!(f, "OpConstant"),
            OpcodeDefinition::OpAdd => write!(f, "OpAdd"),
            OpcodeDefinition::OpPop => write!(f, "OpPop"),
        }
    }
}

pub fn make(opcode: Opcode, operands: Option<&Vec<usize>>) -> Instructions {
    let definition = OpcodeDefinition::lookup(&opcode);
    let instruction_len = definition.widths().iter().fold(1, |len, w| len + w);

    let mut instruction = vec![0; instruction_len];
    instruction[0] = opcode as Byte;

    if let Some(operands) = operands {
        let mut offset = 1;
        for (index, operand) in operands.iter().enumerate() {
            let width = definition.widths()[index];
            match width {
                2 => BigEndian::write_u16(&mut instruction[offset..], *operand as u16),
                _ => todo!(),
            };
            offset += width;
        }
    }

    instruction
}

fn read_operands(
    definition: &OpcodeDefinition,
    instructions: &Instructions,
) -> (Vec<usize>, usize) {
    let mut operands = vec![0; definition.widths().len()];
    let mut offset = 0;

    for (index, width) in definition.widths().iter().enumerate() {
        match width {
            0 => {}
            2 => operands[index] = read_u16(&instructions[offset..]).into(),
            _ => todo!(),
        };
        offset += width;
    }

    (operands, offset)
}

pub fn disasemble(instructions: &Instructions) -> Result<String, CodeError> {
    let mut buffer = String::new();
    let mut index = 0;
    while index < instructions.len() {
        let definition = OpcodeDefinition::lookup_byte(instructions[index])?;
        let (operands, offset) = read_operands(&definition, &instructions[(index + 1)..].to_vec());
        buffer.push_str(
            format!(
                "{:04} {}\n",
                index,
                format_instruction(&definition, &operands)?
            )
            .borrow(),
        );
        index += 1 + offset;
    }

    Ok(buffer)
}

fn format_instruction(
    definition: &OpcodeDefinition,
    operands: &[usize],
) -> Result<String, CodeError> {
    let operand_count = definition.widths().len();
    if operands.len() != operand_count {
        panic!(
            "Expected OpcodeDefinition width: {} to match number of operands: {}",
            operand_count,
            operands.len()
        );
    }
    match operand_count {
        0 => Ok(format!("{}", definition)),
        1 => Ok(format!("{} {}", definition, operands[0])),
        _ => Err(CodeError::NotImplementedYet),
    }
}

pub(crate) fn read_u16(instructions_slice: &[u8]) -> u16 {
    BigEndian::read_u16(instructions_slice)
}

#[cfg(test)]
mod tests {
    use crate::code::read_operands;

    use super::*;

    #[test]
    fn it_makes_bytecode_instructions() {
        let tests = vec![
            (
                Opcode::OpConstant,
                vec![65534],
                vec![Opcode::OpConstant as Byte, 255, 254],
            ),
            (Opcode::OpAdd, vec![], vec![Opcode::OpAdd as Byte]),
        ];

        for (opcode, operands, expected) in tests {
            let instruction = make(opcode, Some(&operands));

            assert_eq!(instruction, expected);
        }
    }

    #[test]
    fn it_makes_bytecode_instructions_string() {
        let expected = r#"0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
"#;
        let instructions = vec![
            make(Opcode::OpAdd, None),
            make(Opcode::OpConstant, Some(&vec![2])),
            make(Opcode::OpConstant, Some(&vec![65535])),
        ]
        .into_iter()
        .flatten()
        .collect::<Instructions>();

        if let Ok(instructions_string) = disasemble(&instructions) {
            assert_eq!(instructions_string, expected);
        } else {
            panic!("Disassembly failed");
        }
    }

    #[test]
    fn read_operands_works() {
        let tests = vec![(Opcode::OpConstant, vec![65535], 2)];

        for (opcode, operands, expected_offset) in tests {
            let instruction = make(opcode, Some(&operands));
            let definition = OpcodeDefinition::lookup(&opcode);
            let (operand, offset) = read_operands(&definition, &instruction[1..].to_vec());
            assert_eq!(offset, expected_offset);
            assert_eq!(operand, operands);
        }
    }
}
