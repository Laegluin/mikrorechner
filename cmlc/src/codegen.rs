//! Generates code from a typed Ast. The generated data structure is close
//! to the final assembler semantics, but must still be converted into actual
//! assembly code.
//!
//! ## Calling convention
//!
//! All standard registers must be saved by the caller.
//!
//! When calling a function, a new stack frame must be initialized by the caller:
//!
//! - the frame pointer must be set to the new stack frame
//! - the frame starts with the return address
//! - followed by the address of where the return value will be constructed
//! - followed by the arguments in correct order

pub mod layout;

use crate::ast::*;
use crate::codegen::layout::*;
use crate::scope_map::ScopeMap;
use crate::span::{Span, Spanned};
use crate::typecheck::{TypeDesc, TypeRef};
use byteorder::{ByteOrder, LittleEndian};
use std::collections::HashMap;
use std::rc::Rc;

pub const ENTRY_POINT: &str = "main";

const STACK_START_ADDR: u32 = 0x0000ffff;
const SET_IMMEDIATE_MAX: u32 = 0b_1_1111_1111_1111_1111_1111;
const LOAD_IMMEDIATE_MAX: u32 = 0b_111_1111_1111_1111;
const STORE_IMMEDIATE_MAX: u32 = 0b_111_1111_1111_1111;

const FRAME_PTR_REG: Reg = Reg::R31;
const TMP_REG: Reg = Reg::R0;
const TMP_OP_REG: Reg = Reg::R1;

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
            cmds: Vec::new(),
            ro_data: HashMap::new(),
        }
    }

    fn push(&mut self, instr: Command) {
        self.cmds.push(instr);
    }

    fn insert_all(&mut self, idx: usize, cmds: &[Command]) {
        assert!(idx < self.cmds.len());

        let mut rest = self.cmds.drain(idx..).collect();
        self.cmds.extend_from_slice(cmds);
        self.cmds.append(&mut rest);
    }

    fn push_const(&mut self, data: impl Into<Vec<u8>>) -> LabelValue {
        let value = LabelValue::new("const");
        self.ro_data.insert(value.label().clone(), data.into());
        value
    }

    pub fn cmds(&self) -> &[Command] {
        &self.cmds
    }

    pub fn ro_data(&self) -> impl Iterator<Item = (&Ident, &[u8])> {
        self.ro_data
            .iter()
            .map(|(label, data)| (label, data.as_slice()))
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
    Jmp(Reg),
    JmpLabel(Ident),
    JmpRel(i32),
    JmpIfLabel(Ident),
    JmpRelIf(u32),
    Load(Reg, Reg, u32),
    Store(Reg, Reg, u32),
    Noop,
    Halt,
    Label(Ident),
    Comment(String),
    EmptyLine,
}

pub fn gen_asm(ast: TypedAst) -> Result<Asm, Spanned<CodegenError>> {
    let mut asm = Asm::new();
    let mut layouts = LayoutCache::new();
    let mut bindings = ScopeMap::new();

    gen_items(&ast.items, &mut bindings, &mut layouts, &ast, &mut asm)?;
    gen_rt_start(&mut asm, &bindings);
    asm.push(Command::Comment(String::from(".ro_data")));
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

    let exit = LabelValue::new("program_exit").label().clone();

    asm.insert_all(
        0,
        &[
            Command::Comment(String::from(".rt_start")),
            // set addr_offset to 0
            Command::Set(Reg::AddrOffset, 0),
            // initialize the stack
            Command::Set(FRAME_PTR_REG, STACK_START_ADDR),
            // set the return address to the last instruction in rt_start (halt)
            Command::SetLabel(TMP_REG, exit.clone()),
            Command::Store(FRAME_PTR_REG, TMP_REG, 0),
            // call main
            Command::JmpLabel(entry_point),
            Command::Label(exit),
            Command::Halt,
            Command::EmptyLine,
            Command::Comment(String::from(".text")),
        ],
    );
}

fn gen_items(
    items: &[Spanned<Item>],
    bindings: &mut ScopeMap<Ident, Value>,
    layouts: &mut LayoutCache,
    ast: &TypedAst,
    asm: &mut Asm,
) -> Result<(), Spanned<CodegenError>> {
    // bind the functions as values first
    for item in items {
        if let Item::FnDef(ref fn_def) = item.value {
            bindings.path_insert(
                fn_def.name.clone(),
                Value::Label(LabelValue::new(fn_def.name.to_string())),
            );
        }
    }

    // then generate the code for them
    for item in items {
        if let Item::FnDef(ref fn_def) = item.value {
            gen_fn(fn_def, bindings, layouts, ast, asm)?;
        }
    }

    Ok(())
}

struct FnContext<'a> {
    ret_addr: &'a Value,
    ret_value: &'a Value,
    regs: RegAllocator,
    stack: StackAllocator,
    bindings: &'a mut ScopeMap<Ident, Value>,
    layouts: &'a mut LayoutCache,
    ast: &'a TypedAst,
}

impl FnContext<'_> {
    /// Allocates space for a value with the given `layout`. Allocates registers if possible and
    /// falls back to stack memory if there are not enough free registers.
    fn alloc(&mut self, layout: &Layout) -> Value {
        self.regs
            .alloc(&layout)
            .map(Value::Reg)
            .unwrap_or_else(|| Value::Stack(self.stack.alloc(&layout)))
    }

    fn layout(&mut self, ty: TypeRef, at: Span) -> Result<Rc<Layout>, Spanned<CodegenError>> {
        self.layouts
            .get_or_gen(ty, self.ast)
            .map_err(|err| Spanned::new(err, at))
    }

    fn enter_scope(&mut self) {
        self.regs.enter_scope();
        self.stack.enter_scope();
        self.bindings.enter_scope();
    }

    fn exit_scope(&mut self) {
        self.regs.exit_scope();
        self.stack.exit_scope();
        self.bindings.exit_scope();
    }
}

fn gen_fn(
    def: &FnDef,
    bindings: &mut ScopeMap<Ident, Value>,
    layouts: &mut LayoutCache,
    ast: &TypedAst,
    asm: &mut Asm,
) -> Result<(), Spanned<CodegenError>> {
    let mut regs = RegAllocator::new();
    let mut stack = StackAllocator::new();
    regs.enter_scope();
    stack.enter_scope();
    bindings.enter_scope();

    // reserve the space used by the return address
    let ret_addr = Value::Stack(stack.alloc(&Layout::word()));

    // bind the return value as dereferenced (out) pointer
    let ret_value = Value::ptr(Value::Stack(stack.alloc(&Layout::word())));

    // bind the arguments
    for param in &def.params {
        let layout = layouts
            .get_or_gen(param.value.ty.clone(), ast)
            .map_err(|err| Spanned::new(err, param.span))?;

        let value = Value::Stack(stack.alloc(&layout));

        // only bind if it has a name; allocation must be done though,
        // because the caller doesn't know the value is unused
        if let Some(ref name) = param.value.name.value {
            bindings.insert(name.clone(), value);
        }
    }

    let mut ctx = FnContext {
        ret_addr: &ret_addr,
        ret_value: &ret_value,
        regs,
        stack,
        bindings,
        layouts,
        ast,
    };

    // generate body
    asm.push(Command::Comment(def.signature()));
    gen_expr(def.body.as_ref(), &ret_value, &mut ctx, asm)?;

    // return to caller (needed if there is no explicit return)
    copy(&ret_addr, &Value::reg(TMP_REG), &Layout::word(), asm);
    asm.push(Command::Jmp(TMP_REG));
    asm.push(Command::EmptyLine);

    ctx.exit_scope();
    Ok(())
}

fn gen_expr(
    expr: Spanned<&Expr>,
    result_value: &Value,
    ctx: &mut FnContext<'_>,
    asm: &mut Asm,
) -> Result<(), Spanned<CodegenError>> {
    let Spanned { value: expr, span } = expr;

    match *expr {
        Expr::Lit(ref lit, ref ty) => {
            let layout = ctx.layout(ty.clone(), span)?;

            match *lit {
                Lit::Bool(is_true) => {
                    let value = if is_true { 1 } else { 0 };

                    if let Some(reg) = result_value.try_get_reg() {
                        asm.push(Command::Set(reg, value));
                    } else {
                        asm.push(Command::Set(TMP_REG, value));
                        copy(&Value::reg(TMP_REG), result_value, &layout, asm);
                    }
                }
                Lit::Int(value) if value <= SET_IMMEDIATE_MAX => {
                    if let Some(reg) = result_value.try_get_reg() {
                        asm.push(Command::Set(reg, value));
                    } else {
                        asm.push(Command::Set(TMP_REG, value));
                        copy(&Value::reg(TMP_REG), result_value, &layout, asm);
                    }
                }
                Lit::Int(value) => {
                    let mut bytes = vec![0; 4];
                    LittleEndian::write_u32(&mut bytes, value);
                    let value = asm.push_const(bytes);

                    if let Some(reg) = result_value.try_get_reg() {
                        asm.push(Command::SetLabel(TMP_REG, value.label().clone()));
                        asm.push(Command::Load(reg, TMP_REG, 0));
                    } else {
                        asm.push(Command::SetLabel(TMP_REG, value.label().clone()));
                        asm.push(Command::Load(TMP_REG, TMP_REG, 0));
                        copy(&Value::reg(TMP_REG), result_value, &layout, asm);
                    }
                }
                Lit::Str(ref string) => {
                    let value = asm.push_const(string.as_bytes());

                    if let Some(reg) = result_value.try_get_reg() {
                        asm.push(Command::SetLabel(TMP_REG, value.label().clone()));
                        asm.push(Command::Load(reg, TMP_REG, 0));
                    } else {
                        asm.push(Command::SetLabel(TMP_REG, value.label().clone()));
                        asm.push(Command::Load(TMP_REG, TMP_REG, 0));
                        copy(&Value::reg(TMP_REG), result_value, &layout, asm);
                    }
                }
            }
        }
        Expr::Var(ref var, ref ty) => {
            let layout = ctx.layout(ty.clone(), span)?;
            let var = ctx.bindings.path_get(var).unwrap();
            copy(var, result_value, &layout, asm);
        }
        Expr::UnOp(UnOp { op, ref operand }, ref ty) => {
            match op {
                UnOpKind::Not => {
                    let operand_value = Value::reg(TMP_REG);
                    gen_expr(operand.as_ref().map(Box::as_ref), &operand_value, ctx, asm)?;

                    let (result_reg, result_needs_copy) =
                        if let Some(reg) = result_value.try_get_reg() {
                            (reg, false)
                        } else {
                            (TMP_REG, true)
                        };

                    asm.push(Command::Not(result_reg, TMP_REG));

                    if result_needs_copy {
                        copy(&Value::reg(result_reg), &result_value, &Layout::word(), asm);
                    }
                }
                UnOpKind::Negate => {
                    let operand_value = Value::reg(TMP_REG);
                    gen_expr(operand.as_ref().map(Box::as_ref), &operand_value, ctx, asm)?;

                    let (result_reg, result_needs_copy) =
                        if let Some(reg) = result_value.try_get_reg() {
                            (reg, false)
                        } else {
                            (TMP_REG, true)
                        };

                    asm.push(Command::Sub(result_reg, Reg::Null, TMP_REG));

                    if result_needs_copy {
                        copy(&Value::reg(result_reg), &result_value, &Layout::word(), asm);
                    }
                }
                UnOpKind::AddrOf | UnOpKind::AddrOfMut => {
                    // allocate the value on the stack, so we can generate a pointer to it
                    let operand_layout = ctx.layout(operand.value.ty().clone(), span)?;
                    let operand_value = ctx.stack.alloc(&operand_layout);
                    let operand_offset = operand_value.offset();

                    gen_expr(
                        operand.as_ref().map(Box::as_ref),
                        &Value::Stack(operand_value),
                        ctx,
                        asm,
                    )?;

                    // set the pointer and copy it to the result value
                    let (result_reg, result_needs_copy) =
                        if let Some(reg) = result_value.try_get_reg() {
                            (reg, false)
                        } else {
                            (TMP_REG, true)
                        };

                    asm.push(Command::Set(result_reg, operand_offset.0));

                    if result_needs_copy {
                        copy(&Value::reg(result_reg), &result_value, &Layout::word(), asm);
                    }
                }
                UnOpKind::Deref => {
                    let result_layout = ctx.layout(ty.clone(), span)?;

                    // load the pointer and copy its content to the result value
                    let operand_value = Value::reg(TMP_REG);
                    gen_expr(operand.as_ref().map(Box::as_ref), &operand_value, ctx, asm)?;
                    let ptr = Value::ptr(operand_value);
                    copy(&ptr, result_value, &result_layout, asm);
                }
            }
        }
        Expr::BinOp(
            BinOp {
                op,
                ref lhs,
                ref rhs,
            },
            ..
        ) => {
            let layout = Layout::word();

            let (result_reg, result_needs_copy) = if let Some(reg) = result_value.try_get_reg() {
                (reg, false)
            } else {
                (TMP_REG, true)
            };

            let lhs_value = Value::reg(TMP_REG);
            gen_expr(lhs.as_ref().map(Box::as_ref), &lhs_value, ctx, asm)?;
            let rhs_value = Value::reg(TMP_OP_REG);
            gen_expr(rhs.as_ref().map(Box::as_ref), &rhs_value, ctx, asm)?;

            match op {
                BinOpKind::Or => asm.push(Command::Or(result_reg, TMP_REG, TMP_OP_REG)),
                BinOpKind::And => asm.push(Command::And(result_reg, TMP_REG, TMP_OP_REG)),
                BinOpKind::Eq => {
                    asm.push(Command::CmpEq(TMP_REG, TMP_OP_REG));
                    asm.push(Command::JmpRelIf(12));
                    asm.push(Command::Set(result_reg, 0));
                    asm.push(Command::JmpRel(8));
                    asm.push(Command::Set(result_reg, 1));
                }
                BinOpKind::Ne => {
                    asm.push(Command::CmpEq(TMP_REG, TMP_OP_REG));
                    asm.push(Command::JmpRelIf(12));
                    asm.push(Command::Set(result_reg, 1));
                    asm.push(Command::JmpRel(8));
                    asm.push(Command::Set(result_reg, 0));
                }
                BinOpKind::Lt => {
                    asm.push(Command::CmpGe(TMP_REG, TMP_OP_REG));
                    asm.push(Command::JmpRelIf(12));
                    asm.push(Command::Set(result_reg, 1));
                    asm.push(Command::JmpRel(8));
                    asm.push(Command::Set(result_reg, 0));
                }
                BinOpKind::Le => {
                    asm.push(Command::CmpGt(TMP_REG, TMP_OP_REG));
                    asm.push(Command::JmpRelIf(12));
                    asm.push(Command::Set(result_reg, 1));
                    asm.push(Command::JmpRel(8));
                    asm.push(Command::Set(result_reg, 0));
                }
                BinOpKind::Gt => {
                    asm.push(Command::CmpGt(TMP_REG, TMP_OP_REG));
                    asm.push(Command::JmpRelIf(12));
                    asm.push(Command::Set(result_reg, 0));
                    asm.push(Command::JmpRel(8));
                    asm.push(Command::Set(result_reg, 1));
                }
                BinOpKind::Ge => {
                    asm.push(Command::CmpGe(TMP_REG, TMP_OP_REG));
                    asm.push(Command::JmpRelIf(12));
                    asm.push(Command::Set(result_reg, 0));
                    asm.push(Command::JmpRel(8));
                    asm.push(Command::Set(result_reg, 1));
                }
                BinOpKind::Add => asm.push(Command::Add(result_reg, TMP_REG, TMP_OP_REG)),
                BinOpKind::Sub => asm.push(Command::Sub(result_reg, TMP_REG, TMP_OP_REG)),
                BinOpKind::Mul => asm.push(Command::Mul(result_reg, TMP_REG, TMP_OP_REG)),
                BinOpKind::Div => asm.push(Command::Div(result_reg, TMP_REG, TMP_OP_REG)),
            }

            if result_needs_copy {
                copy(&Value::reg(result_reg), &result_value, &layout, asm);
            }
        }
        Expr::FnCall(ref call, ref ty) => {
            let ret_layout = ctx.layout(ty.clone(), span)?;
            let ret_value = ctx.stack.alloc(&ret_layout);
            let ret_addr = LabelValue::new("ret_addr");
            let ret_addr_label = ret_addr.label().clone();

            // generate code for all the args
            let mut arg_values = Vec::with_capacity(call.args.len());

            for arg in &call.args {
                let arg_expr = arg.value.value.as_ref();
                let arg_layout = ctx.layout(arg_expr.value.ty().clone(), span)?;
                let arg_value = Value::Stack(ctx.stack.alloc(&arg_layout));

                gen_expr(arg_expr, &arg_value, ctx, asm)?;
                arg_values.push((arg_value, arg_layout));
            }

            ctx.stack.enter_scope();

            // save the registers; we can do this in the new scope, since
            // we don't need them after restoring them again
            for reg in ctx.regs.allocated_regs() {
                let save = Value::Stack(ctx.stack.alloc(&Layout::word()));
                copy(&Value::reg(reg), &save, &Layout::word(), asm);
            }

            // the new stack frame will start here
            let new_frame_offset = ctx.stack.frame_offset();

            // write the return address
            let ret_addr_value = Value::Stack(ctx.stack.alloc(&Layout::word()));
            copy(
                &Value::Label(ret_addr),
                &ret_addr_value,
                &Layout::word(),
                asm,
            );

            // write the address for the return value
            asm.push(Command::Set(TMP_REG, ret_value.offset().0));
            asm.push(Command::Add(TMP_REG, FRAME_PTR_REG, TMP_REG));
            let ret_value_addr = Value::Stack(ctx.stack.alloc(&Layout::word()));
            copy(&Value::reg(TMP_REG), &ret_value_addr, &Layout::word(), asm);

            // copy the args to the new stack frame
            for (arg_value, arg_layout) in arg_values {
                let value = Value::Stack(ctx.stack.alloc(&arg_layout));
                copy(&arg_value, &value, &arg_layout, asm);
            }

            ctx.stack.exit_scope();

            // set the frame pointer, load the function address and then call the function
            asm.push(Command::Set(TMP_REG, new_frame_offset.0));
            asm.push(Command::Add(FRAME_PTR_REG, FRAME_PTR_REG, TMP_REG));
            let fn_ptr = ctx.bindings.path_get(&call.name.value).unwrap();
            copy(fn_ptr, &Value::reg(TMP_REG), &Layout::word(), asm);
            asm.push(Command::Jmp(TMP_REG));

            // restore the frame pointer
            asm.push(Command::Label(ret_addr_label));
            asm.push(Command::Set(TMP_REG, new_frame_offset.0));
            asm.push(Command::Sub(FRAME_PTR_REG, FRAME_PTR_REG, TMP_REG));

            // restore the saved registers using the temporary values saved to the stack
            // just before the called functions's frame, then throw away the allocations
            // to allow overwriting the no longer needed values
            ctx.stack.enter_scope();

            for reg in ctx.regs.allocated_regs() {
                let save = Value::Stack(ctx.stack.alloc(&Layout::word()));
                copy(&save, &Value::reg(reg), &Layout::word(), asm);
            }

            ctx.stack.exit_scope();
        }
        Expr::MemberAccess(
            MemberAccess {
                ref value,
                ref member,
            },
            ref ty,
        ) => {
            let base_layout = ctx.layout(value.value.ty().clone(), span)?;
            let result_layout = ctx.layout(ty.clone(), span)?;

            let base_value = ctx.alloc(&base_layout);
            gen_expr(value.as_ref().map(Box::as_ref), &base_value, ctx, asm)?;
            let field_value = base_value.unwrap_field(&member.value, &base_layout);
            copy(&field_value, result_value, &result_layout, asm);
        }
        _ => unimplemented!(),
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
        (Value::Label(ref src), Value::Ptr(ref dst)) => {
            copy(dst, &Value::reg(TMP_REG), &Layout::word(), asm);
            asm.push(Command::SetLabel(TMP_OP_REG, src.label().clone()));
            asm.push(Command::Store(TMP_REG, TMP_OP_REG, 0));
        }
        (Value::Stack(ref src), Value::Reg(ref dst)) => {
            assert!(layout.is_uniform());

            for (offset, reg) in (0..layout.stack_size().0).step_by(4).zip(dst.regs()) {
                let offset = src.offset() + StackOffset(offset);
                asm.push(Command::Load(*reg, FRAME_PTR_REG, offset.0));
            }
        }
        (Value::Stack(ref src), Value::Stack(ref dst)) => {
            // need at least four bytes for a copy
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
        (Value::Stack(ref src), Value::Ptr(ref dst)) => {
            copy(dst, &Value::reg(TMP_REG), &Layout::word(), asm);
            asm.push(Command::Load(TMP_OP_REG, FRAME_PTR_REG, src.offset().0));
            asm.push(Command::Store(TMP_REG, TMP_OP_REG, 0));
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
        (Value::Reg(ref src), Value::Ptr(ref dst)) => {
            copy(dst, &Value::reg(TMP_REG), &Layout::word(), asm);
            asm.push(Command::Store(TMP_REG, src.reg(), 0));
        }
        (Value::Ptr(ref src), Value::Stack(ref dst)) => {
            // very similar to stack -> stack, except that the offset is applied
            // directly to the address of the pointer
            assert!(layout.stack_size() >= StackOffset(4));
            copy(src, &Value::reg(TMP_OP_REG), &Layout::word(), asm);

            for offset in (0..layout.stack_size().0).step_by(4) {
                let offset = if offset <= (layout.stack_size().0 - 4) {
                    StackOffset(offset)
                } else {
                    layout.stack_size() - StackOffset(4)
                };

                let src_offset = offset;
                let dst_offset = dst.offset() + offset;

                asm.push(Command::Load(TMP_REG, TMP_OP_REG, src_offset.0));
                asm.push(Command::Store(FRAME_PTR_REG, TMP_REG, dst_offset.0));
            }
        }
        (Value::Ptr(ref src), Value::Reg(ref dst)) => {
            // very similar to stack -> reg, except that the offset is applied
            // directly to the address of the pointer
            assert!(layout.is_uniform());
            copy(src, &Value::reg(TMP_REG), &Layout::word(), asm);

            for (offset, reg) in (0..layout.stack_size().0).step_by(4).zip(dst.regs()) {
                asm.push(Command::Load(*reg, TMP_REG, offset));
            }
        }
        (Value::Ptr(ref src), Value::Ptr(ref dst)) => {
            // very similar to stack -> stack, except that both base addresses
            // are the respective pointers
            // because of that, there is quite a bit of loading to make due with only
            // two temporary registers
            assert!(layout.stack_size() >= StackOffset(4));

            for offset in (0..layout.stack_size().0).step_by(4) {
                let offset = if offset <= (layout.stack_size().0 - 4) {
                    offset
                } else {
                    layout.stack_size().0 - 4
                };

                copy(src, &Value::reg(TMP_OP_REG), &Layout::word(), asm);
                asm.push(Command::Load(TMP_REG, TMP_OP_REG, offset));
                copy(dst, &Value::reg(TMP_OP_REG), &Layout::word(), asm);
                asm.push(Command::Store(TMP_OP_REG, TMP_REG, offset));
            }
        }
    }
}
