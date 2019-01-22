use crate::ast::*;
use crate::codegen::ENTRY_POINT;
use crate::span::{Span, Spanned};
use crate::typecheck::unify::TypeEnv;
use crate::typecheck::{Type, TypeError, TypeRef};
use fnv::FnvHashMap;

pub fn verify_types(mut ast: Ast) -> Result<TypedAst, Spanned<TypeError>> {
    let mut types = FnvHashMap::default();

    for item in &mut ast.items {
        let Spanned { value: item, .. } = item;

        match *item {
            Item::FnDef(FnDef {
                ref mut params,
                ref mut ret_ty,
                ref mut body,
                ref name,
                ..
            }) => {
                for Spanned { value: param, span } in params.iter_mut() {
                    canonicalize_type_ref(&mut param.ty, &mut ast.type_env, &mut types, *span)?;
                }

                canonicalize_type_ref(ret_ty, &mut ast.type_env, &mut types, body.span)?;
                verify_expr(body.as_mut(), &mut ast.type_env, &mut types)?;

                if *name.name() == ENTRY_POINT {
                    verify_entry_point(params, ret_ty, &ast.type_env, name.span())?;
                }
            }
            _ => (),
        }
    }

    Ok(TypedAst {
        items: ast.items,
        types,
    })
}

fn verify_expr(
    expr: Spanned<&mut Expr>,
    type_env: &TypeEnv,
    types: &mut FnvHashMap<TypeRef, Type>,
) -> Result<(), Spanned<TypeError>> {
    let Spanned { value: expr, span } = expr;

    match *expr {
        Expr::Lit(_, ref mut ty) | Expr::Var(_, ref mut ty) => {
            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::UnOp(
            UnOp {
                ref mut operand, ..
            },
            ref mut ty,
        ) => {
            verify_expr(operand.as_mut().map(Box::as_mut), type_env, types)?;
            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::BinOp(
            BinOp {
                ref mut lhs,
                ref mut rhs,
                ..
            },
            ref mut ty,
        ) => {
            verify_expr(lhs.as_mut().map(Box::as_mut), type_env, types)?;
            verify_expr(rhs.as_mut().map(Box::as_mut), type_env, types)?;
            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::FnCall(FnCall { ref mut args, .. }, ref mut ty) => {
            for arg in args {
                verify_expr(arg.value.value.as_mut(), type_env, types)?;
            }

            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::MemberAccess(MemberAccess { ref mut value, .. }, ref mut ty) => {
            verify_expr(value.as_mut().map(Box::as_mut), type_env, types)?;
            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::ArrayCons(ArrayCons { ref mut elems }, ref mut ty) => {
            for elem in elems {
                verify_expr(elem.as_mut(), type_env, types)?;
            }

            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::TupleCons(TupleCons { ref mut elems }, ref mut ty) => {
            for elem in elems {
                verify_expr(elem.as_mut(), type_env, types)?;
            }

            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::Assignment(
            Assignment {
                ref mut target,
                ref mut value,
            },
            ref mut ty,
        ) => {
            verify_expr(target.as_mut().map(Box::as_mut), type_env, types)?;
            verify_expr(value.as_mut().map(Box::as_mut), type_env, types)?;
            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::LetBinding(
            LetBinding {
                ref mut expr,
                ref mut pattern,
                ..
            },
            ref mut ty,
        ) => {
            verify_pattern(pattern.as_mut(), type_env, types)?;
            verify_expr(expr.as_mut().map(Box::as_mut), type_env, types)?;
            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::AutoRef(ref mut expr, ref mut ty) => {
            verify_expr(Spanned::new(expr.as_mut(), span), type_env, types)?;
            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::Ret(ref mut expr, ref mut ty) => {
            if let Some(expr) = expr {
                verify_expr(expr.as_mut().map(Box::as_mut), type_env, types)?;
            }

            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::IfExpr(
            IfExpr {
                ref mut cond,
                ref mut then_block,
                ref mut else_block,
            },
            ref mut ty,
        ) => {
            verify_expr(cond.as_mut().map(Box::as_mut), type_env, types)?;
            verify_expr(then_block.as_mut().map(Box::as_mut), type_env, types)?;

            if let Some(else_block) = else_block {
                verify_expr(else_block.as_mut().map(Box::as_mut), type_env, types)?;
            }

            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::Block(Block { ref mut exprs, .. }, ref mut ty) => {
            for expr in exprs {
                verify_expr(expr.as_mut(), type_env, types)?;
            }

            canonicalize_type_ref(ty, type_env, types, span)?;
        }
        Expr::ConstructRecord(ref mut ty) | Expr::ConstructVariants(_, ref mut ty) => {
            canonicalize_type_ref(ty, type_env, types, span)?;
        }
    }

    Ok(())
}

fn verify_pattern(
    pattern: Spanned<&mut Pattern>,
    type_env: &TypeEnv,
    types: &mut FnvHashMap<TypeRef, Type>,
) -> Result<(), Spanned<TypeError>> {
    match *pattern.value {
        Pattern::Discard(ref mut ty) => {
            canonicalize_type_ref(ty, type_env, types, pattern.span)?;
        }
        Pattern::Binding(_, ref mut ty) | Pattern::MutBinding(_, ref mut ty) => {
            canonicalize_type_ref(ty, type_env, types, pattern.span)?;
        }
        Pattern::Tuple(ref mut patterns, ref mut ty) => {
            canonicalize_type_ref(ty, type_env, types, pattern.span)?;

            for pattern in patterns {
                verify_pattern(pattern.as_mut(), type_env, types)?;
            }
        }
    }

    Ok(())
}

fn verify_entry_point(
    params: &[Spanned<ParamDef>],
    ret_ty_ref: &TypeRef,
    type_env: &TypeEnv,
    span: Span,
) -> Result<(), Spanned<TypeError>> {
    let ret_ty = type_env.find_type(&ret_ty_ref).1;

    if !params.is_empty() || !ret_ty.is_unit() {
        Err(Spanned::new(
            TypeError::EntryPointTypeMismatch(ret_ty_ref.desc()),
            span,
        ))
    } else {
        Ok(())
    }
}

fn canonicalize_type_ref(
    ty_ref: &mut TypeRef,
    type_env: &TypeEnv,
    types: &mut FnvHashMap<TypeRef, Type>,
    span: Span,
) -> Result<(), Spanned<TypeError>> {
    let (canonical_ref, ty) = type_env.find_type(ty_ref);

    match *ty {
        Type::Var | Type::PartialRecord(_) | Type::Ptr(_) => Err(Spanned::new(
            TypeError::CannotInfer(canonical_ref.desc()),
            span,
        )),
        // if the int type does not matter, default to i32
        Type::Int => {
            let canonical_ref = canonical_ref.with_desc(&Type::I32);

            types
                .entry(canonical_ref.clone())
                .or_insert_with(|| Type::I32);

            *ty_ref = canonical_ref;
            Ok(())
        }
        _ => {
            types
                .entry(canonical_ref.clone())
                .or_insert_with(|| ty.clone());

            *ty_ref = canonical_ref;
            Ok(())
        }
    }
}
