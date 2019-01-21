use crate::codegen::layout::Reg;
use crate::codegen::{Asm, Command};
use std::fmt::{self, Display};

impl Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Reg::*;

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

impl Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Command::*;

        match *self {
            Add(dst, lhs, rhs) => write!(f, "{} = {} + {}", dst, lhs, rhs),
            Sub(dst, lhs, rhs) => write!(f, "{} = {} + {}", dst, lhs, rhs),
            Mul(dst, lhs, rhs) => write!(f, "{} = {} + {}", dst, lhs, rhs),
            Div(dst, lhs, rhs) => write!(f, "{} = {} + {}", dst, lhs, rhs),
            And(dst, lhs, rhs) => write!(f, "{} = {} & {}", dst, lhs, rhs),
            Or(dst, lhs, rhs) => write!(f, "{} = {} | {}", dst, lhs, rhs),
            Not(dst, src) => write!(f, "{} = ~{}", dst, src,),
            Xor(dst, lhs, rhs) => write!(f, "{} = {} ^ {}", dst, lhs, rhs),
            ShiftL(dst, lhs, rhs) => write!(f, "{} = {} << {} times", dst, lhs, rhs),
            ShiftR(dst, lhs, rhs) => write!(f, "{} = {} >> {} times", dst, lhs, rhs),
            SignedShiftR(dst, lhs, rhs) => write!(f, "{} = {} >>_s {} times", dst, lhs, rhs),
            Copy(dst, src) => write!(f, "copy {} to {}", src, dst,),
            Set(dst, imm) => write!(f, "{} = {}", dst, imm,),
            // TODO: syntax?
            SetLabel(dst, ref label) => write!(f, "{} = {}", dst, label,),
            CmpEq(lhs, rhs) => write!(f, "compare {} = {}", lhs, rhs,),
            CmpGt(lhs, rhs) => write!(f, "compare {} > {}", lhs, rhs,),
            CmpGe(lhs, rhs) => write!(f, "compare {} >= {}", lhs, rhs,),
            Jmp(addr) => write!(f, "jump to {}", addr,),
            JmpLabel(ref label) => write!(f, "jump to {}", label,),
            JmpRel(offset) => write!(f, "jump_rel to {}", offset,),
            JmpIfLabel(ref label) => write!(f, "jump_if to {}", label,),
            JmpRelIf(offset) => write!(f, "jump_rel_if to {}", offset,),
            Load(dst, src_addr, offset) => write!(f, "load {} + {} to {}", src_addr, offset, dst),
            // TODO: syntax looks wrong?
            Store(dst_addr, src, offset) => write!(f, "store {} + {} to {}", src, offset, dst_addr),
            Noop => write!(f, "noop"),
            Halt => write!(f, "halt"),
            Label(ref label) => write!(f, "_{}", label),
            Comment(ref comment) => write!(f, "# {}", comment),
            EmptyLine => write!(f, ""),
        }
    }
}

pub fn emit_asm(asm: &Asm) -> String {
    let mut buf = String::new();

    let mut label = None;

    for cmd in asm.cmds() {
        if let Command::Label(_) = cmd {
            label = Some(cmd);
        } else {
            match label.take() {
                Some(label) => buf.push_str(&format!("{} {}\n", cmd, label)),
                None => buf.push_str(&format!("{}\n", cmd.to_string())),
            }
        }
    }

    for (label, data) in asm.ro_data() {
        buf.push_str(&format!("0x{} {}", hex::encode(data), label,));
    }

    buf
}
