use memory::Word;
use memory::{Memory, OP_CODE_BITS, REG_REF_BITS, WORD_BITS, WORD_BYTES};
use num_enum::CustomTryInto;
use rand;
use std::fmt::{self, Display};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Index, IndexMut, Mul, Shl, Shr, Sub};
use std::sync::atomic::{AtomicBool, Ordering};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use {Error, ErrorContext, ErrorKind};

pub struct RegBank {
    next_instr_addr: Word,
    regs: [Word; 34],
    cmp_flag: bool,
}

impl RegBank {
    pub fn new() -> RegBank {
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

impl Display for RegBank {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "next instruction: {:#x}", self.next_instr_addr)?;
        writeln!(f, "comparison flag: {}", self.cmp_flag)?;

        for reg in Reg::iter() {
            writeln!(f, "{}: {val:#x} ({val})", reg, val = self[reg])?;
        }

        Ok(())
    }
}

#[derive(strum_macros::Display, EnumIter, CustomTryInto, Clone, Copy)]
#[repr(u8)]
#[rustfmt::skip]
#[strum(serialize_all = "snake_case")]
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
    fn from_word(word: Word, pos: RegPos) -> Result<Reg, ErrorKind> {
        // shift to right so the register is in the lowest bits
        let shift_by = WORD_BITS - (OP_CODE_BITS + (pos as Word + 1 * REG_REF_BITS));
        let rest = word >> shift_by;

        let mask = Word::max_value() >> (WORD_BITS - REG_REF_BITS);
        let reg = (rest & mask) as u8;

        match reg.try_into_Reg() {
            Ok(reg) => Ok(reg),
            Err(_) => Err(ErrorKind::IllegalRegister(reg)),
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
    fn from_word(word: Word) -> Result<Op, ErrorKind> {
        let code = word >> (WORD_BITS - OP_CODE_BITS);

        match (code as u8).try_into_Op() {
            // TODO: mul and div are not implemented yet
            Ok(op) => match op {
                Op::Mul | Op::Div => Err(ErrorKind::IllegalInstruction(word)),
                op => Ok(op),
            },
            Err(_) => Err(ErrorKind::IllegalInstruction(word)),
        }
    }
}

fn immediate_from_word(word: Word, num_reg_refs: Word) -> Word {
    let mask = Word::max_value() >> (OP_CODE_BITS + (num_reg_refs * REG_REF_BITS));
    word & mask
}

pub struct Breakpoints(Vec<Word>);

impl Breakpoints {
    pub fn new() -> Breakpoints {
        Breakpoints(Vec::new())
    }

    pub fn push(&mut self, addr: Word) {
        self.0
            .binary_search(&addr)
            .map_err(|idx| self.0.insert(idx, addr));
    }

    pub fn is_breakpoint(&self, addr: Word) -> bool {
        self.0.binary_search(&addr).is_ok()
    }
}

impl Display for Breakpoints {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.is_empty() {
            return write!(f, "<none>");
        }

        for breakpoint in &self.0 {
            writeln!(f, "{:#x}", breakpoint)?;
        }

        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Status {
    Ready,
    Halt,
}

pub fn run(
    regs: &mut RegBank,
    mem: &mut Memory,
    breakpoints: &Breakpoints,
    pause: &AtomicBool,
) -> Result<Status, Error> {
    while !pause.load(Ordering::Acquire) {
        if breakpoints.is_breakpoint(regs.next_instr_addr()) {
            return Ok(Status::Ready);
        }

        if run_next(regs, mem)? == Status::Halt {
            return Ok(Status::Halt);
        }
    }

    Ok(Status::Ready)
}

fn run_next(regs: &mut RegBank, mem: &mut Memory) -> Result<Status, Error> {
    use self::Op::*;

    let instr_addr = regs.next_instr_addr();
    regs.incr_instr_addr();
    let err_ctx = ErrorContext::at(instr_addr);

    let instr = err_ctx.map_err(mem.load_word(instr_addr))?;

    match err_ctx.map_err(Op::from_word(instr))? {
        Add => err_ctx.map_err(binary_op(instr, Word::add, regs)),
        Sub => err_ctx.map_err(binary_op(instr, Word::sub, regs)),
        Mul => err_ctx.map_err(binary_op(instr, Word::mul, regs)),
        Div => err_ctx.map_err(binary_op(instr, Word::div, regs)),
        And => err_ctx.map_err(binary_op(instr, Word::bitand, regs)),
        Or => err_ctx.map_err(binary_op(instr, Word::bitor, regs)),
        Not => err_ctx.map_err(not(instr, regs)),
        Xor => err_ctx.map_err(binary_op(instr, Word::bitxor, regs)),
        ShiftL => err_ctx.map_err(binary_op(instr, Word::shl, regs)),
        ShiftR => err_ctx.map_err(binary_op(instr, Word::shr, regs)),
        SignedShiftR => err_ctx.map_err(binary_op(instr, |l, r| (l as i32 >> r) as Word, regs)),
        Copy => err_ctx.map_err(copy(instr, regs)),
        Set => err_ctx.map_err(set(instr, regs)),
        CmpEq => err_ctx.map_err(cmp(instr, |l, r| l == r, regs)),
        CmpGt => err_ctx.map_err(cmp(instr, |l, r| l > r, regs)),
        CmpGe => err_ctx.map_err(cmp(instr, |l, r| l >= r, regs)),
        Jmp => err_ctx.map_err(jmp(instr, regs)),
        JmpRel => err_ctx.map_err(jmp_rel(instr, regs)),
        JmpIf => if regs.is_cmp_set() {
            err_ctx.map_err(jmp(instr, regs))
        } else {
            Ok(Status::Ready)
        },
        JmpRelIf => if regs.is_cmp_set() {
            err_ctx.map_err(jmp_rel(instr, regs))
        } else {
            Ok(Status::Ready)
        },
        Load => err_ctx.map_err(load(instr, regs, mem)),
        Store => err_ctx.map_err(store(instr, regs, mem)),
        NoOp => Ok(Status::Ready),
        Halt => Ok(Status::Halt),
    }
}

fn binary_op<F>(instr: Word, op: F, regs: &mut RegBank) -> Result<Status, ErrorKind>
where
    F: FnOnce(Word, Word) -> Word,
{
    let dst = Reg::from_word(instr, RegPos::Dst)?;
    let lhs = Reg::from_word(instr, RegPos::Arg1)?;
    let rhs = Reg::from_word(instr, RegPos::Arg2)?;

    regs[dst] = op(regs[lhs], regs[rhs]);
    Ok(Status::Ready)
}

fn not(instr: Word, regs: &mut RegBank) -> Result<Status, ErrorKind> {
    let dst = Reg::from_word(instr, RegPos::Dst)?;
    let src = Reg::from_word(instr, RegPos::Arg1)?;
    regs[dst] = !regs[src];
    Ok(Status::Ready)
}

fn copy(instr: Word, regs: &mut RegBank) -> Result<Status, ErrorKind> {
    let dst = Reg::from_word(instr, RegPos::Dst)?;
    let src = Reg::from_word(instr, RegPos::Arg1)?;
    regs[dst] = regs[src];
    Ok(Status::Ready)
}

fn set(instr: Word, regs: &mut RegBank) -> Result<Status, ErrorKind> {
    let dst = Reg::from_word(instr, RegPos::Dst)?;
    regs[dst] = immediate_from_word(instr, 1);
    Ok(Status::Ready)
}

fn cmp<F>(instr: Word, op: F, regs: &mut RegBank) -> Result<Status, ErrorKind>
where
    F: FnOnce(Word, Word) -> bool,
{
    let lhs = Reg::from_word(instr, RegPos::Arg1)?;
    let rhs = Reg::from_word(instr, RegPos::Arg2)?;

    regs.cmp_flag = op(regs[lhs], regs[rhs]);
    Ok(Status::Ready)
}

fn jmp(instr: Word, regs: &mut RegBank) -> Result<Status, ErrorKind> {
    let addr = regs[Reg::from_word(instr, RegPos::Dst)?];
    regs.next_instr_addr = addr;
    Ok(Status::Ready)
}

fn jmp_rel(instr: Word, regs: &mut RegBank) -> Result<Status, ErrorKind> {
    let offset = immediate_from_word(instr, 0);
    // subtract the word len, because the instr_addr already points at the next instruction
    // TODO: allow negative offsets?
    regs.next_instr_addr = regs.next_instr_addr + offset - WORD_BYTES;
    Ok(Status::Ready)
}

fn load(instr: Word, regs: &mut RegBank, mem: &mut Memory) -> Result<Status, ErrorKind> {
    let dst = Reg::from_word(instr, RegPos::Dst)?;
    let src_addr_reg = Reg::from_word(instr, RegPos::Arg1)?;
    let offset = immediate_from_word(instr, 2);

    regs[dst] = mem.load_word(regs[src_addr_reg] + offset)?;
    Ok(Status::Ready)
}

fn store(instr: Word, regs: &mut RegBank, mem: &mut Memory) -> Result<Status, ErrorKind> {
    let dst_addr_reg = Reg::from_word(instr, RegPos::Dst)?;
    let src = Reg::from_word(instr, RegPos::Arg1)?;
    let offset = immediate_from_word(instr, 2);

    mem.store_word(regs[dst_addr_reg] + offset, regs[src]);
    Ok(Status::Ready)
}
