//! Generates code from a typed Ast. The generated data structure is close
//! to the final assembler semantics, but must still be converted into actual
//! assembly code.
//!
//! ## Calling convention
//!
//! All standard registers must be saved by the caller. The callee may clobber
//! any register except for the frame pointer, which must be restored to the
//! its previous value.
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
use crate::emit::Command;
use crate::scope_map::ScopeMap;
use crate::emit::Reg;
use crate::span::{Span, Spanned};
use crate::typecheck::{TypeDesc, TypeRef};
use byteorder::{ByteOrder, LittleEndian};
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::rc::Rc;
use crate::emit::Label;

pub const ENTRY_POINT: &str = "main";

const STACK_START_ADDR: u32 = 0x80000000;
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

impl Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use CodegenError::*;

        match *self {
            UnsizedType(ref ty) => write!(
                f,
                "cannot refer to unsized type `{}` without pointer indirection",
                ty
            ),
            InfiniteSize(ref ty) => write!(f, "type `{}` has an infinite size", ty),
        }
    }
}

#[derive(Debug)]
pub struct Asm {
    rt_start: Vec<Command>,
    text: Vec<Command>,
    ro_data: HashMap<Vec<u8>, LabelValue>,
}

impl Asm {
    fn new() -> Asm {
        Asm {
            rt_start: Vec::new(),
            text: Vec::new(),
            ro_data: HashMap::new(),
        }
    }

    fn set_rt_start(&mut self, rt_start: Vec<Command>) {
        self.rt_start = rt_start;
    }

    fn push(&mut self, instr: Command) {
        self.text.push(instr);
    }

    fn push_const_u32(&mut self, value: u32) -> LabelValue {
        let mut bytes = vec![0; 4];
        LittleEndian::write_u32(&mut bytes, value);
        self.push_const(bytes)
    }

    fn push_const(&mut self, data: impl Into<Vec<u8>>) -> LabelValue {
        self.ro_data
            .entry(data.into())
            .or_insert_with(|| LabelValue::new("const"))
            .clone()
    }

    pub fn rt_start(&self) -> &[Command] {
        &self.rt_start
    }

    pub fn text(&self) -> &[Command] {
        &self.text
    }

    pub fn ro_data(&self) -> impl Iterator<Item = (&Label, &[u8])> {
        self.ro_data
            .iter()
            .map(|(data, label)| (label.label(), data.as_slice()))
    }
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
    let entry_point = entry_point.unwrap_label().label().clone();
    let exit = LabelValue::new("program_exit").label().clone();
    let stack_start = asm.push_const_u32(STACK_START_ADDR).label().clone();

    asm.set_rt_start(vec![
        // set addr_offset to 0
        Command::Set(Reg::AddrOffset, 0),
        // initialize the stack
        Command::SetLabel(TMP_REG, stack_start),
        Command::Load(FRAME_PTR_REG, TMP_REG, 0),
        // set the return address to the last instruction in rt_start (halt)
        Command::SetLabel(TMP_REG, exit.clone()),
        Command::Store(FRAME_PTR_REG, TMP_REG, 0),
        // call main
        Command::JmpLabel(entry_point),
        Command::Label(exit),
        Command::Halt,
    ]);
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
    ret_layout: &'a Layout,
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
    let fn_label = bindings
        .path_get(&def.name)
        .unwrap()
        .unwrap_label()
        .label()
        .clone();

    let mut regs = RegAllocator::new();
    let mut stack = StackAllocator::new();
    regs.enter_scope();
    stack.enter_scope();
    bindings.enter_scope();

    // reserve the space used by the return address
    let ret_addr = Value::Stack(stack.alloc(&Layout::word()));

    // bind the return value as dereferenced (out) pointer
    let ret_layout = layouts
        .get_or_gen(def.ret_ty.clone(), ast)
        .map_err(|err| Spanned::new(err, def.name.span()))?;

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
        ret_layout: &ret_layout,
        regs,
        stack,
        bindings,
        layouts,
        ast,
    };

    // generate body
    asm.push(Command::Comment(def.signature()));
    asm.push(Command::Label(fn_label));
    gen_expr(def.body.as_ref(), &ret_value, &ret_layout, &mut ctx, asm)?;

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
    result_layout: &Layout,
    ctx: &mut FnContext<'_>,
    asm: &mut Asm,
) -> Result<(), Spanned<CodegenError>> {
    let Spanned { value: expr, span } = expr;

    match *expr {
        Expr::Lit(ref lit, _) => match *lit {
            Lit::Bool(is_true) => {
                let value = if is_true { 1 } else { 0 };

                if let Some(reg) = result_value.try_get_reg() {
                    asm.push(Command::Set(reg, value));
                } else {
                    asm.push(Command::Set(TMP_REG, value));
                    copy(&Value::reg(TMP_REG), result_value, &result_layout, asm);
                }
            }
            Lit::Int(value) if value <= SET_IMMEDIATE_MAX => {
                if let Some(reg) = result_value.try_get_reg() {
                    asm.push(Command::Set(reg, value));
                } else {
                    asm.push(Command::Set(TMP_REG, value));
                    copy(&Value::reg(TMP_REG), result_value, &result_layout, asm);
                }
            }
            Lit::Int(value) => {
                let value = asm.push_const_u32(value);

                if let Some(reg) = result_value.try_get_reg() {
                    asm.push(Command::SetLabel(TMP_REG, value.label().clone()));
                    asm.push(Command::Load(reg, TMP_REG, 0));
                } else {
                    asm.push(Command::SetLabel(TMP_REG, value.label().clone()));
                    asm.push(Command::Load(TMP_REG, TMP_REG, 0));
                    copy(&Value::reg(TMP_REG), result_value, &result_layout, asm);
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
                    copy(&Value::reg(TMP_REG), result_value, &result_layout, asm);
                }
            }
        },
        Expr::Var(ref var, _) => {
            let var = ctx.bindings.path_get(var).unwrap();
            copy(var, result_value, &result_layout, asm);
        }
        Expr::UnOp(UnOp { op, ref operand }, _) => {
            match op {
                UnOpKind::Not => {
                    let operand_value = Value::reg(TMP_REG);
                    gen_expr(
                        operand.as_ref().map(Box::as_ref),
                        &operand_value,
                        &Layout::word(),
                        ctx,
                        asm,
                    )?;

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
                    gen_expr(
                        operand.as_ref().map(Box::as_ref),
                        &operand_value,
                        &Layout::word(),
                        ctx,
                        asm,
                    )?;

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
                        &operand_layout,
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

                    asm.push(Command::Set(TMP_REG, operand_offset.0));
                    asm.push(Command::Add(result_reg, FRAME_PTR_REG, TMP_REG));

                    if result_needs_copy {
                        copy(&Value::reg(result_reg), &result_value, &Layout::word(), asm);
                    }
                }
                UnOpKind::Deref => {
                    // load the pointer and copy its content to the result value
                    let operand_value = Value::reg(TMP_REG);
                    gen_expr(
                        operand.as_ref().map(Box::as_ref),
                        &operand_value,
                        &Layout::word(),
                        ctx,
                        asm,
                    )?;

                    let ptr = Value::ptr(operand_value);
                    copy(&ptr, result_value, &result_layout, asm);
                }
            }
        }
        Expr::Cast(Cast { ref expr, .. }, _) => {
            // cast simply reinterpret the value, so no code must be generated
            gen_expr(
                expr.as_ref().map(Box::as_ref),
                result_value,
                result_layout,
                ctx,
                asm,
            )?;
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

            gen_expr(
                lhs.as_ref().map(Box::as_ref),
                &Value::reg(TMP_REG),
                &Layout::word(),
                ctx,
                asm,
            )?;

            gen_expr(
                rhs.as_ref().map(Box::as_ref),
                &Value::reg(TMP_OP_REG),
                &Layout::word(),
                ctx,
                asm,
            )?;

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

                gen_expr(arg_expr, &arg_value, &arg_layout, ctx, asm)?;
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

            // copy the result to the target (we cannot build in place because the result
            // might be discarded, but the called function does not know that)
            copy(&Value::Stack(ret_value), result_value, &ret_layout, asm);
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
            gen_expr(
                value.as_ref().map(Box::as_ref),
                &base_value,
                &base_layout,
                ctx,
                asm,
            )?;

            let field_value = base_value.unwrap_field(&member.value, &base_layout);
            copy(&field_value, result_value, &result_layout, asm);
        }
        Expr::ArrayCons(ArrayCons { ref elems }, ref ty) => {
            let arr_layout = ctx.layout(ty.clone(), span)?;

            for (idx, elem) in elems.iter().enumerate() {
                let elem_layout = ctx.layout(elem.value.ty().clone(), elem.span)?;
                let elem_value = result_value.unwrap_field(idx, &arr_layout);
                gen_expr(elem.as_ref(), &elem_value, &elem_layout, ctx, asm)?;
            }
        }
        Expr::TupleCons(TupleCons { ref elems }, ref ty) => {
            let tuple_layout = ctx.layout(ty.clone(), span)?;

            for (idx, elem) in elems.iter().enumerate() {
                let elem_layout = ctx.layout(elem.value.ty().clone(), elem.span)?;
                let elem_value = result_value.unwrap_field(idx, &tuple_layout);
                gen_expr(elem.as_ref(), &elem_value, &elem_layout, ctx, asm)?;
            }
        }
        Expr::Assignment(
            Assignment {
                ref target,
                ref value,
            },
            ..
        ) => {
            let target_layout = ctx.layout(target.value.ty().clone(), target.span)?;
            let target_value = ctx.alloc(&target_layout);
            let value_layout = ctx.layout(value.value.ty().clone(), value.span)?;
            let value_value = ctx.alloc(&value_layout);

            gen_expr(
                target.as_ref().map(Box::as_ref),
                &target_value,
                &target_layout,
                ctx,
                asm,
            )?;

            gen_expr(
                value.as_ref().map(Box::as_ref),
                &value_value,
                &value_layout,
                ctx,
                asm,
            )?;
            copy(&value_value, &target_value, &target_layout, asm);

            // ignore the result value since it is always ()
        }
        Expr::LetBinding(
            LetBinding {
                ref pattern,
                ref expr,
                ..
            },
            ..
        ) => {
            let layout = ctx.layout(expr.value.ty().clone(), expr.span)?;
            let value = ctx.alloc(&layout);
            gen_expr(expr.as_ref().map(Box::as_ref), &value, &layout, ctx, asm)?;
            bind_pattern(&pattern.value, value, &layout, ctx)?;

            // ignore the result value since it is always ()
        }
        Expr::AutoRef(ref expr, _) => {
            // not implemented right now, so simply forward auto refs
            gen_expr(
                Spanned::new(expr, span),
                result_value,
                result_layout,
                ctx,
                asm,
            )?;
        }
        Expr::Ret(ref expr, _) => {
            // store the result in the return value slot, if there's one
            if let Some(ref expr) = expr {
                gen_expr(
                    expr.as_ref().map(Box::as_ref),
                    &ctx.ret_value,
                    &ctx.ret_layout,
                    ctx,
                    asm,
                )?;
            }

            // jump to caller
            copy(&ctx.ret_addr, &Value::reg(TMP_REG), &Layout::word(), asm);
            asm.push(Command::Jmp(TMP_REG));
        }
        Expr::IfExpr(
            IfExpr {
                ref cond,
                ref then_block,
                ref else_block,
            },
            _,
        ) => {
            gen_expr(
                cond.as_ref().map(Box::as_ref),
                &Value::reg(TMP_REG),
                &Layout::word(),
                ctx,
                asm,
            )?;

            let else_label = LabelValue::new("else").label().clone();
            asm.push(Command::CmpEq(TMP_REG, Reg::Null));
            asm.push(Command::JmpIfLabel(else_label.clone()));

            gen_expr(
                then_block.as_ref().map(Box::as_ref),
                result_value,
                result_layout,
                ctx,
                asm,
            )?;

            let end_if_label = LabelValue::new("end_if").label().clone();
            asm.push(Command::JmpLabel(end_if_label.clone()));
            asm.push(Command::Label(else_label));

            if let Some(ref else_block) = *else_block {
                gen_expr(
                    else_block.as_ref().map(Box::as_ref),
                    result_value,
                    result_layout,
                    ctx,
                    asm,
                )?;
            }

            asm.push(Command::Noop); // avoid two consecutive labels if there's no else block
            asm.push(Command::Label(end_if_label));
        }
        Expr::Block(Block { ref exprs, .. }, _) => {
            ctx.enter_scope();

            for expr in exprs.iter().rev().skip(1).rev() {
                gen_expr(
                    expr.as_ref(),
                    &Value::zero_sized(),
                    &Layout::zero_sized(),
                    ctx,
                    asm,
                )?;
            }

            if let Some(expr) = exprs.last() {
                gen_expr(expr.as_ref(), &result_value, &result_layout, ctx, asm)?;
            }

            ctx.exit_scope();
        }
        Expr::ConstructRecord(ref ty) => {
            // reinterpret the args as the return type, then copy it into the return value
            let layout = ctx.layout(ty.clone(), span)?;
            let value = Value::Stack(StackAllocator::new().alloc(&layout));
            copy(&value, ctx.ret_value, &layout, asm);
        }
    }

    Ok(())
}

fn bind_pattern(
    pattern: &Pattern,
    value: Value,
    layout: &Layout,
    ctx: &mut FnContext<'_>,
) -> Result<(), Spanned<CodegenError>> {
    match *pattern {
        Pattern::Discard(_) => (),
        Pattern::Binding(ref name, _) | Pattern::MutBinding(ref name, _) => {
            ctx.bindings.insert(name.value.clone(), value);
        }
        Pattern::Tuple(ref patterns, ref ty) => {
            for (idx, elem_pattern) in patterns.iter().enumerate() {
                let elem_value = value.unwrap_field(idx, layout);
                let elem_layout = ctx.layout(ty.clone(), elem_pattern.span)?;
                bind_pattern(&elem_pattern.value, elem_value, &elem_layout, ctx)?;
            }
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
