use byteorder::{BigEndian, ByteOrder};
use std::{borrow::Borrow, fmt};

pub type OpcodeSize = u8;
pub type Instructions = [OpcodeSize];

pub fn print(instructions: &Instructions) -> Result<String, ()> {
    let mut buffer = String::new();
    let mut index = 0;

    while index < instructions.len() {
        let opcode = Opcode::lookup(instructions[index])?;
        let definition = OpcodeDefinition::lookup(&opcode);
        let (operands, offset) = read_operands(&definition, &instructions[(index + 1)..]);
        buffer.push_str(format!("{:04} {}\n", index, format(&definition, &operands)?).borrow());
        index += 1 + offset;
    }

    return Ok(buffer);
}

fn format(definition: &OpcodeDefinition, operands: &[usize]) -> Result<String, ()> {
    let operand_count = definition.widths().len();
    if operands.len() != operand_count {
        return Err(());
    }
    match operand_count {
        1 => Ok(format!("{} {}", definition, operands[0])),
        _ => Err(()),
    }
}

// Opcodes are represented by a single byte.
#[repr(u8)]
pub enum Opcode {
    OpConstant = 0,
}

impl Opcode {
    pub fn lookup(op: OpcodeSize) -> Result<Self, ()> {
        match op {
            0 => Ok(Opcode::OpConstant),
            _ => Err(())
        }
    }
}

// Definitions map Opcodes to the number of bytes each operand takes up.
pub enum OpcodeDefinition<'operand> {
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

pub fn make(op: Opcode, operands: &[usize]) -> Vec<OpcodeSize> {
    let definition = OpcodeDefinition::lookup(&op);
    let instruction_len = definition.widths().iter().fold(1, |len, w| len + w);

    let mut instruction: Vec<OpcodeSize> = vec![0; instruction_len];
    instruction[0] = op as OpcodeSize;

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

fn read_operands(
    definition: &OpcodeDefinition,
    instructions: &Instructions,
) -> (Vec<usize>, usize) {
    let mut operands = vec![0, definition.widths().len()];
    let mut offset = 0;

    for (index, width) in definition.widths().iter().enumerate() {
        match width {
            2 => operands[index] = BigEndian::read_u16(&instructions[offset..]) as usize,
            _ => {}
        };
        offset += width
    }

    (operands, offset)
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

    #[test]
    fn it_makes_bytecode_instructions_string() {
        let instructions = vec![
            make(Opcode::OpConstant, &[1]),
            make(Opcode::OpConstant, &[2]),
            make(Opcode::OpConstant, &[65535]),
        ];
        let instructions_string = instructions
            .iter()
            .map(|instr| print(instr).unwrap())
            .collect::<Vec<String>>()
            .join("\n");
        let expected = r#"0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535"#;

        assert_eq!(instructions_string, expected);
    }
}
