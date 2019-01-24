use crate::codegen::Asm;
use byteorder::{ByteOrder, LittleEndian};
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::rc::Rc;
use strum_macros::EnumIter;

pub const JMP_REL_IMMEDIATE_MAX: u32 = 0b_111_1111_1111_1111_1111_1111_1111;
pub const JMP_REL_IF_IMMEDIATE_MAX: u32 = 0b_111_1111_1111_1111_1111_1111_1111;
pub const SET_IMMEDIATE_MAX: u32 = 0b_1_1111_1111_1111_1111_1111;
pub const LOAD_IMMEDIATE_MAX: u32 = 0b_111_1111_1111_1111;
pub const STORE_IMMEDIATE_MAX: u32 = 0b_111_1111_1111_1111;

pub fn emit_asm(asm: Asm) -> String {
    let mut buf = String::new();
    let mut label = None;

    for cmd in asm.commands() {
        if cmd.is_label() {
            label = Some(cmd);
        } else {
            match label.take() {
                Some(label) => buf.push_str(&format!("{} {}\n", cmd, label)),
                None => buf.push_str(&format!("{}\n", cmd.to_string())),
            }
        }
    }

    buf
}

pub fn emit_object(asm: Asm) -> Vec<u8> {
    let mut labels = HashMap::new();
    let mut idx = 0;

    for cmd in asm.commands() {
        if let Command::Label(ref label) = &cmd {
            labels.insert(label.clone(), idx);
        }

        idx += cmd.len();
    }

    let mut img = Vec::new();
    let mut idx = 0;

    for cmd in asm.commands() {
        cmd.write_img(&mut img, idx, &labels);
        idx += cmd.len();
    }

    img
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum Command {
    Add(Reg, Reg, Reg),
    Sub(Reg, Reg, Reg),
    Mul(Reg, Reg, Reg),
    Div(Reg, Reg, Reg),
    And(Reg, Reg, Reg),
    Or(Reg, Reg, Reg),
    Not(Reg, Reg),
    Xor(Reg, Reg, Reg),
    ShiftL(Reg, Reg, Reg),
    ShiftR(Reg, Reg, Reg),
    SignedShiftR(Reg, Reg, Reg),
    Copy(Reg, Reg),
    Set(Reg, u32),
    SetLabel(Reg, Label),
    CmpEq(Reg, Reg),
    CmpGt(Reg, Reg),
    CmpGe(Reg, Reg),
    Jmp(Reg),
    JmpRelLabel(Label),
    JmpRel(i32),
    JmpRelIfLabel(Label),
    JmpRelIf(i32),
    Load(Reg, Reg, u32),
    Store(Reg, Reg, u32),
    Noop,
    Halt,
    Label(Label),
    Data(Rc<[u8]>),
    Comment(String),
    EmptyLine,
}

impl Command {
    fn is_label(&self) -> bool {
        match *self {
            Command::Label(_) => true,
            _ => false,
        }
    }

    fn len(&self) -> u32 {
        use self::Command::*;

        match *self {
            Label(_) | Comment(_) | EmptyLine => 0,
            Data(ref data) => data.len() as u32,
            _ => 4,
        }
    }

    fn write_img(&self, img: &mut Vec<u8>, idx: u32, labels: &HashMap<Label, u32>) {
        use self::Command::*;

        let mut bytes = vec![0; 4];

        match *self {
            Add(dst, lhs, rhs) => {
                let instr = Instruction::op(Op::Add).dst(dst).arg1(lhs).arg2(rhs).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            Sub(dst, lhs, rhs) => {
                let instr = Instruction::op(Op::Sub).dst(dst).arg1(lhs).arg2(rhs).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            Mul(dst, lhs, rhs) => {
                let instr = Instruction::op(Op::Mul).dst(dst).arg1(lhs).arg2(rhs).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            Div(dst, lhs, rhs) => {
                let instr = Instruction::op(Op::Div).dst(dst).arg1(lhs).arg2(rhs).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            And(dst, lhs, rhs) => {
                let instr = Instruction::op(Op::And).dst(dst).arg1(lhs).arg2(rhs).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            Or(dst, lhs, rhs) => {
                let instr = Instruction::op(Op::Or).dst(dst).arg1(lhs).arg2(rhs).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            Not(dst, src) => {
                let instr = Instruction::op(Op::Add).dst(dst).arg1(src).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            Xor(dst, lhs, rhs) => {
                let instr = Instruction::op(Op::Xor).dst(dst).arg1(lhs).arg2(rhs).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            ShiftL(dst, lhs, rhs) => {
                let instr = Instruction::op(Op::ShiftL)
                    .dst(dst)
                    .arg1(lhs)
                    .arg2(rhs)
                    .get();

                LittleEndian::write_u32(&mut bytes, instr);
            }
            ShiftR(dst, lhs, rhs) => {
                let instr = Instruction::op(Op::ShiftR)
                    .dst(dst)
                    .arg1(lhs)
                    .arg2(rhs)
                    .get();

                LittleEndian::write_u32(&mut bytes, instr);
            }
            SignedShiftR(dst, lhs, rhs) => {
                let instr = Instruction::op(Op::SignedShiftR)
                    .dst(dst)
                    .arg1(lhs)
                    .arg2(rhs)
                    .get();

                LittleEndian::write_u32(&mut bytes, instr);
            }
            Copy(dst, src) => {
                let instr = Instruction::op(Op::Copy).dst(dst).arg1(src).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            Set(dst, imm) => {
                let instr = Instruction::op(Op::Set)
                    .dst(dst)
                    .immediate(imm, SET_IMMEDIATE_MAX.count_ones())
                    .get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            SetLabel(dst, ref label) => {
                let instr = Instruction::op(Op::Set)
                    .dst(dst)
                    .immediate(labels[label], 21)
                    .get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            CmpEq(lhs, rhs) => {
                let instr = Instruction::op(Op::CmpEq).arg1(lhs).arg2(rhs).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            CmpGt(lhs, rhs) => {
                let instr = Instruction::op(Op::CmpGt).arg1(lhs).arg2(rhs).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            CmpGe(lhs, rhs) => {
                let instr = Instruction::op(Op::CmpGe).arg1(lhs).arg2(rhs).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            Jmp(addr) => {
                let instr = Instruction::op(Op::Jmp).dst(addr).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            JmpRelLabel(ref label) => {
                let offset = labels[label] as i32 - idx as i32;

                let instr = Instruction::op(Op::JmpRel)
                    .signed_immediate(offset, JMP_REL_IMMEDIATE_MAX.count_ones())
                    .get();

                LittleEndian::write_u32(&mut bytes, instr);
            }
            JmpRel(offset) => {
                let instr = Instruction::op(Op::JmpRel)
                    .signed_immediate(offset, JMP_REL_IMMEDIATE_MAX.count_ones())
                    .get();

                LittleEndian::write_u32(&mut bytes, instr);
            }
            JmpRelIfLabel(ref label) => {
                let offset = labels[label] as i32 - idx as i32;

                let instr = Instruction::op(Op::JmpRelIf)
                    .signed_immediate(offset, JMP_REL_IF_IMMEDIATE_MAX.count_ones())
                    .get();

                LittleEndian::write_u32(&mut bytes, instr);
            }
            JmpRelIf(offset) => {
                let instr = Instruction::op(Op::JmpRelIf)
                    .signed_immediate(offset, JMP_REL_IF_IMMEDIATE_MAX.count_ones())
                    .get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            Load(dst, src_addr, offset) => {
                let instr = Instruction::op(Op::Load)
                    .dst(dst)
                    .arg1(src_addr)
                    .immediate(offset, LOAD_IMMEDIATE_MAX.count_ones())
                    .get();

                LittleEndian::write_u32(&mut bytes, instr);
            }
            Store(dst_addr, src, offset) => {
                let instr = Instruction::op(Op::Load)
                    .dst(dst_addr)
                    .arg1(src)
                    .immediate(offset, STORE_IMMEDIATE_MAX.count_ones())
                    .get();

                LittleEndian::write_u32(&mut bytes, instr);
            }
            Noop => {
                let instr = Instruction::op(Op::Noop).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            Halt => {
                let instr = Instruction::op(Op::Halt).get();
                LittleEndian::write_u32(&mut bytes, instr);
            }
            Label(_) => (),
            Data(ref data) => {
                bytes.clear();
                bytes.extend_from_slice(data);
            }
            Comment(_) => (),
            EmptyLine => (),
        }

        img.append(&mut bytes);
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Command::*;

        match *self {
            Add(dst, lhs, rhs) => write!(f, "{} = {} + {}", dst, lhs, rhs),
            Sub(dst, lhs, rhs) => write!(f, "{} = {} - {}", dst, lhs, rhs),
            Mul(dst, lhs, rhs) => write!(f, "{} = {} * {}", dst, lhs, rhs),
            Div(dst, lhs, rhs) => write!(f, "{} = {} / {}", dst, lhs, rhs),
            And(dst, lhs, rhs) => write!(f, "{} = {} & {}", dst, lhs, rhs),
            Or(dst, lhs, rhs) => write!(f, "{} = {} | {}", dst, lhs, rhs),
            Not(dst, src) => write!(f, "{} = ~{}", dst, src,),
            Xor(dst, lhs, rhs) => write!(f, "{} = {} ^ {}", dst, lhs, rhs),
            ShiftL(dst, lhs, rhs) => write!(f, "{} = {} << {} times", dst, lhs, rhs),
            ShiftR(dst, lhs, rhs) => write!(f, "{} = {} >> {} times", dst, lhs, rhs),
            SignedShiftR(dst, lhs, rhs) => write!(f, "{} = {} >>_s {} times", dst, lhs, rhs),
            Copy(dst, src) => write!(f, "copy {} to {}", src, dst,),
            Set(dst, imm) => write!(f, "{} = {}", dst, imm,),
            SetLabel(dst, ref label) => write!(f, "{} = {}", dst, label,),
            CmpEq(lhs, rhs) => write!(f, "compare {} = {}", lhs, rhs,),
            CmpGt(lhs, rhs) => write!(f, "compare {} > {}", lhs, rhs,),
            CmpGe(lhs, rhs) => write!(f, "compare {} >= {}", lhs, rhs,),
            Jmp(addr) => write!(f, "jump to {}", addr,),
            JmpRelLabel(ref label) => write!(f, "jump_rel to {}", label,),
            JmpRel(offset) => write!(f, "jump_rel to {}", offset,),
            JmpRelIfLabel(ref label) => write!(f, "jump_rel_if to {}", label,),
            JmpRelIf(offset) => write!(f, "jump_rel_if to {}", offset,),
            Load(dst, src_addr, offset) => write!(f, "load {} + {} to {}", src_addr, offset, dst),
            Store(dst_addr, src, offset) => write!(f, "store {} to {} + {}", src, dst_addr, offset),
            Noop => write!(f, "noop"),
            Halt => write!(f, "halt"),
            Label(ref label) => write!(f, "_{}", label),
            Data(ref data) => write!(f, "0x{}", hex::encode(data)),
            Comment(ref comment) => write!(f, "# {}", comment),
            EmptyLine => write!(f, ""),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label(Rc<str>);

impl Label {
    pub fn new(label: impl AsRef<str>) -> Label {
        Label(Rc::from(label.as_ref()))
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for Label {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, EnumIter)]
#[repr(u8)]
#[rustfmt::skip]
pub enum Reg {
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

impl Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Reg::*;

        let name = match *self {
            R0 => "R0",
            R1 => "R1",
            R2 => "R2",
            R3 => "R3",
            R4 => "R4",
            R5 => "R5",
            R6 => "R6",
            R7 => "R7",
            R8 => "R8",
            R9 => "R9",
            R10 => "R10",
            R11 => "R11",
            R12 => "R12",
            R13 => "R13",
            R14 => "R14",
            R15 => "R15",
            R16 => "R16",
            R17 => "R17",
            R18 => "R18",
            R19 => "R19",
            R20 => "R20",
            R21 => "R21",
            R22 => "R22",
            R23 => "R23",
            R24 => "R24",
            R25 => "R25",
            R26 => "R26",
            R27 => "R27",
            R28 => "R28",
            R29 => "R29",
            R30 => "R30",
            R31 => "R31",
            AddrOffset => "offset",
            Null => "null",
        };

        write!(f, "{}", name)
    }
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
#[rustfmt::skip]
#[allow(unused)]
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
    Noop            = 0b_01100,
    Halt            = 0b_01101,
}

struct Instruction(u32);

impl Instruction {
    fn op(op: Op) -> Instruction {
        Instruction(set_op_code(0, op))
    }

    fn dst(self, reg: Reg) -> Instruction {
        Instruction(set_reg(self.0, reg, RegPos::Dst))
    }

    fn arg1(self, reg: Reg) -> Instruction {
        Instruction(set_reg(self.0, reg, RegPos::Arg1))
    }

    fn arg2(self, reg: Reg) -> Instruction {
        Instruction(set_reg(self.0, reg, RegPos::Arg2))
    }

    fn immediate(self, imm: u32, num_bits: u32) -> Instruction {
        Instruction(set_immediate(self.0, imm, num_bits))
    }

    fn signed_immediate(self, imm: i32, num_bits: u32) -> Instruction {
        Instruction(set_signed_immediate(self.0, imm, num_bits))
    }

    fn get(self) -> u32 {
        self.0
    }
}

// some of these constants should probably be shared with the simulator
fn set_op_code(instr: u32, op: Op) -> u32 {
    let op = op as u32;
    instr | (op << 27)
}

enum RegPos {
    Dst,
    Arg1,
    Arg2,
}

fn set_reg(instr: u32, reg: Reg, pos: RegPos) -> u32 {
    let shift_by = match pos {
        RegPos::Dst => 21,
        RegPos::Arg1 => 15,
        RegPos::Arg2 => 9,
    };

    instr | ((reg as u32) << shift_by)
}

fn set_immediate(instr: u32, imm: u32, num_bits: u32) -> u32 {
    assert!(num_bits <= 32);
    let imm_max = u32::max_value() >> (32 - num_bits);
    assert!(imm <= imm_max);
    instr | imm
}

fn set_signed_immediate(instr: u32, imm: i32, num_bits: u32) -> u32 {
    assert!(num_bits <= 32);
    let imm_max = u32::max_value() >> (32 - num_bits);
    let imm = (imm as u32) & imm_max;

    instr | imm
}
