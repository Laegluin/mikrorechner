use derive_more::{Add, AddAssign};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

const STACK_PTR_REG: Reg = Reg::R31;

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
enum Reg {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    R16,
    R17,
    R18,
    R19,
    R20,
    R21,
    R22,
    R23,
    R24,
    R25,
    R26,
    R27,
    R28,
    R29,
    R30,
    R31,
    AddrOffset,
    Null,
}

impl Reg {
    fn is_general_purpose(&self) -> bool {
        match *self {
            Reg::AddrOffset | Reg::Null | STACK_PTR_REG => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct FieldIdx(usize);

#[derive(Debug, Clone, Copy, Add, AddAssign)]
struct StackOffset(u32);

#[derive(Debug, Clone, Copy, Add, AddAssign)]
struct RegOffset(u8);

trait Layout {
    fn stack_size(&self) -> StackOffset;

    fn stack_field(&self, field: FieldIdx, value: &StackValue) -> StackValue;

    fn reg_size(&self) -> RegOffset;

    fn reg_field_offset(&self, field: FieldIdx) -> RegValue;
}

struct RegValue {
    regs: Vec<Reg>,
}

impl RegValue {
    fn reg(&self) -> Reg {
        self.regs[0]
    }
}

struct StackValue {
    start: StackOffset,
}

impl StackValue {
    fn offset(&self) -> StackOffset {
        self.start
    }
}

struct StackAllocator {
    start: StackOffset,
}

impl StackAllocator {
    fn new() -> StackAllocator {
        StackAllocator {
            start: StackOffset(0),
        }
    }

    fn alloc(&mut self, layout: &Layout) -> StackValue {
        let value = StackValue { start: self.start };
        self.start += layout.stack_size();
        value
    }
}

struct RegAllocator {
    free_regs: Vec<Reg>,
}

impl RegAllocator {
    fn new() -> RegAllocator {
        RegAllocator {
            free_regs: Reg::iter().filter(Reg::is_general_purpose).collect(),
        }
    }

    fn alloc(&mut self, layout: &Layout) -> Option<RegValue> {
        if self.free_regs.len() < layout.reg_size().0 as usize {
            None
        } else {
            let regs = self
                .free_regs
                .drain(0..layout.reg_size().0 as usize)
                .collect();

            Some(RegValue { regs })
        }
    }
}
