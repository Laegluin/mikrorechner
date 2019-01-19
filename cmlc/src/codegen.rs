//! Generates code from a typed Ast. The generated data structure is close
//! to the final assembler semantics, but must still be converted into actual
//! assembly code.
//!
//! ## Calling convention
//!
//! All registers (including the frame pointer) must be saved by the caller
//! as necessary.
//!
//! When calling a function, a new stack frame must be initialized for it:
//!
//! - starting with stack memory reserved for the return value
//! - followed by the return address

mod layout;

use crate::ast::*;
use crate::codegen::layout::*;
use crate::scope_map::ScopeMap;
use crate::span::Spanned;
use crate::typecheck::TypeDesc;
use std::collections::HashMap;
use std::rc::Rc;

pub const ENTRY_POINT: &str = "main";
const NUM_RT_START_COMMANDS: usize = 8;

const STACK_START_ADDR: u32 = 0xffffffff;
const LOAD_IMMEDIATE_MAX: u32 = 0b_111_1111_1111_1111;
const STORE_IMMEDIATE_MAX: u32 = 0b_111_1111_1111_1111;

const FRAME_PTR_REG: Reg = Reg::R31;
const TMP_REG: Reg = Reg::R0;

#[derive(Debug)]
pub enum CodegenError {
    UnsizedType(Rc<TypeDesc>),
    InfiniteSize(Rc<TypeDesc>),
}

#[derive(Debug)]
pub struct Asm {
    cmds: Vec<Command>,
    ro_data: HashMap<Ident, Vec<u8>>,
}

impl Asm {
    fn new() -> Asm {
        Asm {
            cmds: vec![Command::Noop; NUM_RT_START_COMMANDS],
            ro_data: HashMap::new(),
        }
    }

    fn push(&mut self, instr: Command) {
        self.cmds.push(instr);
    }

    fn set_rt_start(&mut self, rt_start: [Command; NUM_RT_START_COMMANDS]) {
        self.cmds[..NUM_RT_START_COMMANDS].clone_from_slice(&rt_start);
    }
}

#[derive(Debug, Clone)]
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
    SetLabel(Reg, Ident),
    CmpEq(Reg, Reg),
    CmpGt(Reg, Reg),
    CmpGe(Reg, Reg),
    JmpLabel(Ident),
    Jmp(Reg),
    JmpRel(u32),
    JmpIf(Ident),
    JmpRelIf(u32),
    Load(Reg, Reg, u32),
    Store(Reg, Reg, u32),
    Noop,
    Halt,
    Comment(String),
    Label(Ident),
}

pub fn gen_asm(ast: TypedAst) -> Result<Asm, Spanned<CodegenError>> {
    let mut asm = Asm::new();
    let mut layouts = LayoutCache::new();
    let mut bindings = ScopeMap::new();

    gen_items(&ast.items, &mut bindings, &mut layouts, &ast, &mut asm)?;
    gen_rt_start(&mut asm, &bindings);
    Ok(asm)
}

fn gen_rt_start(asm: &mut Asm, bindings: &ScopeMap<Ident, Value>) {
    // get the entry point
    // typechecking already made sure it exits and since it is in the global
    // scope, it must be a label
    let entry_point = bindings.get(&Ident::new(ENTRY_POINT)).unwrap();

    let entry_point = match *entry_point {
        Value::Label(ref value) => value.label().clone(),
        _ => unreachable!(),
    };

    let rt_start = [
        Command::Comment(String::from(".rt_start")),
        // set addr_offset to 0
        Command::Set(Reg::AddrOffset, 0),
        // initialize the stack
        Command::Set(FRAME_PTR_REG, STACK_START_ADDR),
        // set the return address to the last instruction in rt_start (halt)
        Command::Set(Reg::R0, 20),
        Command::Store(FRAME_PTR_REG, Reg::R0, 0),
        // call main
        Command::JmpLabel(entry_point),
        Command::Halt,
        Command::Comment(String::from(".text")),
    ];

    asm.set_rt_start(rt_start);
}

fn gen_items(
    items: &[Spanned<Item>],
    bindings: &mut ScopeMap<Ident, Value>,
    layouts: &mut LayoutCache,
    ast: &TypedAst,
    asm: &mut Asm,
) -> Result<(), Spanned<CodegenError>> {
    for item in items {
        let Spanned { value: item, span } = item;

        match item {
            Item::FnDef(ref fn_def) => unimplemented!(),
            _ => continue,
        }
    }

    Ok(())
}

fn copy(src: &Value, dst: &Value, layout: &Layout, asm: &mut Asm) {
    if layout.is_zero_sized() {
        return;
    }

    match (src, dst) {
        (_, Value::Label(_)) => panic!("cannot copy to label"),
        (Value::Label(ref src), Value::Reg(ref dst)) => {
            asm.push(Command::SetLabel(dst.reg(), src.label().clone()));
        }
        (Value::Label(ref src), Value::Stack(ref dst)) => {
            asm.push(Command::SetLabel(TMP_REG, src.label().clone()));
            asm.push(Command::Store(FRAME_PTR_REG, TMP_REG, dst.offset().0));
        }
        (Value::Stack(ref src), Value::Reg(ref dst)) => {
            assert!(layout.is_uniform());

            for (offset, reg) in (0..layout.stack_size().0).step_by(4).zip(dst.regs()) {
                let offset = src.offset() + StackOffset(offset);
                asm.push(Command::Load(*reg, FRAME_PTR_REG, offset.0));
            }
        }
        (Value::Stack(ref src), Value::Stack(ref dst)) => {
            // need at least four bytes a copy
            assert!(layout.stack_size() >= StackOffset(4));

            for offset in (0..layout.stack_size().0).step_by(4) {
                // if the last value is not exactly four bytes, simply copy
                // some bytes again
                let offset = if offset <= (layout.stack_size().0 - 4) {
                    StackOffset(offset)
                } else {
                    layout.stack_size() - StackOffset(4)
                };

                let src_offset = src.offset() + offset;
                let dst_offset = dst.offset() + offset;

                asm.push(Command::Load(TMP_REG, FRAME_PTR_REG, src_offset.0));
                asm.push(Command::Store(FRAME_PTR_REG, TMP_REG, dst_offset.0));
            }
        }
        (Value::Reg(ref src), Value::Stack(ref dst)) => {
            assert!(layout.is_uniform());

            for (offset, reg) in (0..layout.stack_size().0).step_by(4).zip(src.regs()) {
                let offset = dst.offset() + StackOffset(offset);
                asm.push(Command::Store(FRAME_PTR_REG, *reg, offset.0));
            }
        }
        (Value::Reg(ref src), Value::Reg(ref dst)) => {
            for (src, dst) in src.regs().iter().zip(dst.regs()) {
                asm.push(Command::Copy(*src, *dst));
            }
        }
    }
}
