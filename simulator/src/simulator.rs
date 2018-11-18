use memory::Word;
use memory::{OP_CODE_BITS, REG_REF_BITS, WORD_BITS, WORD_BYTES};
use num_enum::CustomTryInto;
use rand;
use std::ops::{Index, IndexMut};
use Error;

struct RegBank {
    next_instr_addr: Word,
    regs: [Word; 34],
    cmp_flag: bool,
}

impl RegBank {
    fn new() -> RegBank {
        // initialize with random values to simulate undefined values
        let mut regs = [0; 34];

        for reg in &mut regs[..] {
            *reg = rand::random();
        }

        RegBank {
            next_instr_addr: 0,
            cmp_flag: rand::random(),
            regs,
        }
    }

    fn incr_instr_addr(&mut self) {
        self.next_instr_addr += WORD_BYTES;
    }

    fn next_instr_addr(&self) -> Word {
        self.next_instr_addr
    }

    fn is_cmp_set(&self) -> bool {
        self.cmp_flag
    }
}

impl Index<Reg> for RegBank {
    type Output = Word;

    fn index(&self, idx: Reg) -> &Word {
        &self.regs[idx as usize]
    }
}

impl IndexMut<Reg> for RegBank {
    fn index_mut(&mut self, idx: Reg) -> &mut Word {
        &mut self.regs[idx as usize]
    }
}

#[derive(CustomTryInto)]
#[repr(u8)]
#[rustfmt::skip]
enum Reg {
    R0          = 0b_000000,
    R1          = 0b_000001,
    R2          = 0b_000010,
    R3          = 0b_000011,
    R4          = 0b_000100,
    R5          = 0b_000101,
    R6          = 0b_000110,
    R7          = 0b_000111,
    R8          = 0b_001000,
    R9          = 0b_001001,
    R10         = 0b_001010,
    R11         = 0b_001011,
    R12         = 0b_001100,
    R13         = 0b_001101,
    R14         = 0b_001110,
    R15         = 0b_001111,
    R16         = 0b_010000,
    R17         = 0b_010001,
    R18         = 0b_010010,
    R19         = 0b_010011,
    R20         = 0b_010100,
    R21         = 0b_010101,
    R22         = 0b_010110,
    R23         = 0b_010111,
    R24         = 0b_011000,
    R25         = 0b_011001,
    R26         = 0b_011010,
    R27         = 0b_011011,
    R28         = 0b_011100,
    R29         = 0b_011101,
    R30         = 0b_011110,
    R31         = 0b_011111,
    Null        = 0b_100000,
    AddrOffset  = 0b_100001,
}

#[derive(Clone, Copy)]
#[repr(u8)]
enum RegPos {
    Dst = 0,
    Arg1 = 1,
    Arg2 = 2,
}

impl Reg {
    #[inline]
    fn from_word(word: Word, pos: RegPos) -> Result<Reg, Error> {
        // shift to right so the register is in the lowest bits
        let shift_by = WORD_BITS - (OP_CODE_BITS + (pos as Word + 1 * REG_REF_BITS));
        let rest = word >> shift_by;

        let mask = Word::max_value() >> (WORD_BITS - REG_REF_BITS);
        let reg = (rest & mask) as u8;

        match reg.try_into_Reg() {
            Ok(reg) => Ok(reg),
            Err(_) => Err(Error::IllegalRegister(reg)),
        }
    }
}

#[derive(CustomTryInto)]
#[repr(u8)]
#[rustfmt::skip]
enum Op {
    Add             = 0b_00000,
    Sub             = 0b_10111,
    Mul             = 0b_01110,
    Div             = 0b_01111,
    And             = 0b_10000,
    Or              = 0b_10001,
    Not             = 0b_10010,
    Xor             = 0b_10011,
    ShiftL          = 0b_10100,
    ShiftR          = 0b_10101,
    SignedShiftR    = 0b_10110,
    Copy            = 0b_00001,
    Set             = 0b_00010,
    CmpEq           = 0b_00011,
    CmpGt           = 0b_00100,
    CmpGe           = 0b_00101,
    Jmp             = 0b_00110,
    JmpRel          = 0b_00111,
    JmpIf           = 0b_01000,
    JmpRelIf        = 0b_01001,
    Load            = 0b_01010,
    Store           = 0b_01011,
    NoOp            = 0b_01100,
    Halt            = 0b_01101,
}

impl Op {
    #[inline]
    fn from_word(word: Word) -> Result<Op, Error> {
        let code = word >> (WORD_BITS - OP_CODE_BITS);

        match (code as u8).try_into_Op() {
            Ok(op) => match op {
                Op::Mul | Op::Div => Err(Error::IllegalInstruction(word)),
                op => Ok(op),
            },
            Err(_) => Err(Error::IllegalInstruction(word)),
        }
    }
}

fn immediate_from_word(word: Word, num_args: Word) -> Word {
    let mask = Word::max_value() >> (OP_CODE_BITS + (num_args * REG_REF_BITS));
    word & mask
}
