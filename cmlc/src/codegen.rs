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
use crate::codegen::layout::{
    LabelValue, Layout, LayoutCache, Reg, StackAllocator, StackValue, Value,
};
use crate::scope_map::ScopeMap;
use crate::span::Spanned;
use crate::typecheck::TypeDesc;
use std::collections::HashMap;
use std::rc::Rc;

pub const ENTRY_POINT: &str = "main";
const NUM_RT_START_COMMANDS: usize = 8;

const STACK_START_ADDR: u32 = 200_000;
const LOAD_IMMEDIATE_MAX: u32 = 0b_111_1111_1111_1111;

const STACK_FRAME_PTR_REG: Reg = Reg::R31;
const TMP_RESULT_REG: Reg = Reg::R0;
const TMP_OPERAND_REG: Reg = Reg::R1;

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
        Value::Label(LabelValue { ref label }) => label.clone(),
        _ => unreachable!(),
    };

    let rt_start = [
        Command::Comment(String::from(".rt_start")),
        // set addr_offset to 0
        Command::Set(Reg::AddrOffset, 0),
        // initialize the stack
        Command::Set(STACK_FRAME_PTR_REG, STACK_START_ADDR),
        // set the return address to the last instruction in rt_start (halt)
        Command::Set(Reg::R0, 20),
        Command::Store(STACK_FRAME_PTR_REG, Reg::R0, 0),
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
