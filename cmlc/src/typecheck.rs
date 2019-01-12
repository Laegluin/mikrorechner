pub mod lint;
pub mod scope_map;
pub mod unify;

use crate::ast::*;
use crate::span::{Span, Spanned};
use crate::support;
use crate::typecheck::scope_map::ScopeMap;
use crate::typecheck::unify::TypeEnv;
use std::hash::{Hash, Hasher};
use std::mem;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug)]
pub enum TypeError {
    HoleInTypeDef,
    UndefinedType(ItemPath),
    UndefinedVariable(ItemPath),
    DuplicatParamName(Ident),
    DuplicatFieldName(Ident),
    UnknownFieldName(Ident),
    VarNotMut(Mutability),
    UnknownArgName(Ident),
    ArityMismatch(usize, usize),
    Mismatch(Rc<TypeName>, Rc<TypeName>),
    NonLValueInAssignment,
    CannotInfer(Rc<TypeName>),
    EntryPointTypeMismatch(Rc<TypeName>),
}

#[derive(Debug, Clone)]
pub struct TypeRef(usize, Rc<TypeName>);

impl TypeRef {
    fn new(id: usize, ty: &Type) -> TypeRef {
        TypeRef(id, Rc::new(TypeName::from_type(ty)))
    }

    pub fn invalid() -> TypeRef {
        TypeRef(
            usize::max_value(),
            Rc::new(TypeName::Name(Ident::new("{invalid}"))),
        )
    }
}

impl PartialEq for TypeRef {
    fn eq(&self, rhs: &TypeRef) -> bool {
        self.0 == rhs.0
    }
}

impl Eq for TypeRef {}

impl Hash for TypeRef {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.0.hash(state)
    }
}

#[derive(Debug)]
pub enum TypeName {
    Hole,
    Name(Ident),
    ConstPtr(Rc<TypeName>),
    MutPtr(Rc<TypeName>),
    Array(Rc<TypeName>, u32),
    Function(Vec<Rc<TypeName>>, Rc<TypeName>),
    Tuple(Vec<Rc<TypeName>>),
    RecordFields(Vec<(Ident, Rc<TypeName>)>),
}

impl TypeName {
    fn from_type(ty: &Type) -> TypeName {
        match *ty {
            Type::Var => TypeName::Hole,
            Type::Int => TypeName::Name(Ident::new("{integer}")),
            Type::Never => TypeName::Name(Ident::new("!")),
            Type::Bool => TypeName::Name(Ident::new("bool")),
            Type::I32 => TypeName::Name(Ident::new("i32")),
            Type::U32 => TypeName::Name(Ident::new("u32")),
            Type::Str => TypeName::Name(Ident::new("str")),
            Type::Ptr(ref ty_ref) => TypeName::ConstPtr(ty_ref.1.clone()),
            Type::ConstPtr(ref ty_ref) => TypeName::ConstPtr(ty_ref.1.clone()),
            Type::MutPtr(ref ty_ref) => TypeName::MutPtr(ty_ref.1.clone()),
            Type::Array(ref ty_ref, len) => TypeName::Array(ty_ref.1.clone(), len),
            Type::Tuple(ref ty_refs) => {
                TypeName::Tuple(ty_refs.iter().map(|ty_ref| ty_ref.1.clone()).collect())
            }
            Type::Function(ref func) => TypeName::Function(
                func.params.iter().map(|ty_ref| ty_ref.1.clone()).collect(),
                func.ret.1.clone(),
            ),
            Type::RecordFields(ref fields) => TypeName::RecordFields(
                fields
                    .iter()
                    .map(|field| (field.name.clone(), field.ty.1.clone()))
                    .collect(),
            ),
            Type::Record(ref record) => TypeName::Name(record.name.clone()),
            Type::Variants(ref variants) => TypeName::Name(variants.name.clone()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    /// A free type variable. All instances of this type will be removed during typechecking.
    Var,
    /// An unspecified integer type. All instances of this type will be removed during typechecking.
    Int,
    Never,
    Bool,
    I32,
    U32,
    Str,
    /// A pointer, either const `*T` or mut `*mut T`. All instances of this type will
    /// be removed during typechecking.
    Ptr(TypeRef),
    ConstPtr(TypeRef),
    MutPtr(TypeRef),
    Array(TypeRef, u32),
    Tuple(Vec<TypeRef>),
    Function(Function),
    /// A record with zero or more fields. All instances of this type will be removed during typechecking.
    RecordFields(Vec<Field>),
    Record(Record),
    Variants(Variants),
}

impl Type {
    pub fn unit() -> Type {
        Type::Tuple(Vec::new())
    }

    pub fn is_unit(&self) -> bool {
        match *self {
            Type::Tuple(ref elems) if elems.is_empty() => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<TypeRef>,
    pub ret: TypeRef,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub id: TypeId,
    pub name: Ident,
    pub fields: Vec<Field>,
}

impl PartialEq for Record {
    fn eq(&self, rhs: &Record) -> bool {
        self.id == rhs.id
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Ident,
    pub ty: TypeRef,
}

#[derive(Debug, Clone)]
pub struct Variants {
    pub id: TypeId,
    pub name: Ident,
    pub variants: Vec<Variant>,
}

impl PartialEq for Variants {
    fn eq(&self, rhs: &Variants) -> bool {
        self.id == rhs.id
    }
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: Ident,
    pub params: Vec<TypeRef>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeId(usize);

impl TypeId {
    pub fn new() -> TypeId {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

        let id = NEXT_ID.fetch_add(1, Ordering::AcqRel);

        if id == usize::max_value() {
            panic!("overflow for type ids");
        }

        TypeId(id)
    }
}

struct Binding {
    ty: TypeRef,
    is_mut: bool,
    param_names: Option<Vec<Option<Ident>>>,
}

impl Binding {
    fn new(ty: TypeRef) -> Binding {
        Binding {
            ty,
            is_mut: false,
            param_names: None,
        }
    }

    fn new_mut(ty: TypeRef) -> Binding {
        Binding {
            ty,
            is_mut: true,
            param_names: None,
        }
    }

    fn new_fn(ty: TypeRef, param_names: Vec<Option<Ident>>) -> Binding {
        Binding {
            ty,
            is_mut: false,
            param_names: Some(param_names),
        }
    }
}

pub fn typecheck(mut ast: Ast) -> Result<TypedAst, Spanned<TypeError>> {
    let mut type_bindings = ScopeMap::new();
    bind_primitives(&mut ast.type_env, &mut type_bindings);
    let mut value_bindings = ScopeMap::new();

    check_items(
        &mut ast.items,
        &mut ast.type_env,
        &mut type_bindings,
        &mut value_bindings,
    )?;

    lint::verify_types(ast)
}

fn check_items(
    items: &mut [Spanned<Item>],
    type_env: &mut TypeEnv,
    type_bindings: &mut ScopeMap<Ident, TypeRef>,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<(), Spanned<TypeError>> {
    // collect all type and function defs, but delay resolution to allow for (mutally) recursive definitions
    // TODO: detect shadowed types and shadowed fns
    for item in &*items {
        match item.value {
            Item::TypeDef(TypeDef::Alias(Spanned {
                value: AliasDef { ref name, .. },
                ..
            }))
            | Item::TypeDef(TypeDef::RecordDef(Spanned {
                value: RecordDef { ref name, .. },
                ..
            }))
            | Item::TypeDef(TypeDef::VariantsDef(Spanned {
                value: VariantsDef { ref name, .. },
                ..
            })) => {
                type_bindings.insert(name.value.clone(), type_env.insert(Type::Var));
            }
            Item::FnDef(FnDef {
                ref name,
                ref params,
                ..
            }) => {
                let ty = type_env.insert(Type::Var);

                // collect the param names to attach them to the binding
                let mut param_names = Vec::with_capacity(params.len());

                for param in params {
                    let name = &param.value.name.value;

                    // do not allow duplicate param names
                    if name.is_some() && param_names.contains(name) {
                        return Err(Spanned::new(
                            TypeError::DuplicatParamName(name.clone().unwrap()),
                            param.value.name.span,
                        ));
                    }

                    param_names.push(name.clone());
                }

                value_bindings.insert(name.value.clone(), Binding::new_fn(ty, param_names));
            }
        }
    }

    // Resolve types; Function body unification is delayed so the type defs can be fully resolved first
    // (otherwise we could get type errors at the definition, causing the unwraps to fail
    // and generally giving poor error messages anyway). In addition, type resolution can
    // insert value into the current scopes (constructor functions), which may be referenced
    // by functions.
    for item in &*items {
        match item.value {
            Item::TypeDef(TypeDef::Alias(Spanned {
                value: AliasDef { ref name, ref ty },
                ..
            })) => {
                // TODO: also alias the constructors
                let ty = type_from_desc(ty.as_ref(), true, type_env, type_bindings)?;
                let var = &type_bindings.get(&name.value).unwrap();
                type_env.unify(&var, &ty).unwrap();
            }
            Item::TypeDef(TypeDef::RecordDef(Spanned { value: ref def, .. })) => {
                bind_record_def(def, type_env, type_bindings, value_bindings)?
            }
            Item::TypeDef(TypeDef::VariantsDef(Spanned { value: ref def, .. })) => {
                bind_variants_def(def, type_env, type_bindings, value_bindings)?
            }
            Item::FnDef(_) => (),
        }
    }

    // start the actual unification for each function
    for item in items {
        match item.value {
            Item::FnDef(ref mut def) => check_fn(def, type_env, type_bindings, value_bindings)?,
            _ => (),
        }
    }

    Ok(())
}

fn check_fn(
    def: &mut FnDef,
    type_env: &mut TypeEnv,
    type_bindings: &mut ScopeMap<Ident, TypeRef>,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<(), Spanned<TypeError>> {
    // get the function type before entering the new scope
    // the type binding must already have been created by `check_items`
    let current_ty = value_bindings.get(&def.name.value).unwrap().ty.clone();

    type_bindings.enter_scope();
    value_bindings.enter_scope();

    let mut params = Vec::with_capacity(def.params.len());

    for param in &mut def.params {
        let param = &mut param.value;

        param.ty = match param.ty_hint.as_ref() {
            Some(hint) => type_from_desc(hint.as_ref(), false, type_env, type_bindings)?,
            None => type_env.insert(Type::Var),
        };

        // bind the parameter if it has a name
        if let Some(ref name) = &param.name.value {
            value_bindings.insert(name.clone(), Binding::new(param.ty.clone()));
        }

        params.push(param.ty.clone());
    }

    def.ret_ty = match def.ret_ty_hint.as_ref() {
        Some(ty) => type_from_desc(ty.as_ref(), false, type_env, type_bindings)?,
        None => type_env.insert(Type::Var),
    };

    // update the functions type
    // since it was just a variable before, unification cannot fail
    let fn_ty = type_env.insert(Type::Function(Function {
        params,
        ret: def.ret_ty.clone(),
    }));

    let ty = type_env.insert(Type::ConstPtr(fn_ty));
    type_env.unify(&current_ty, &ty).unwrap();

    // check the function body
    let actual_ret_ty = check_expr(
        def.body.as_mut(),
        &def.ret_ty,
        Mutability::Const,
        type_env,
        type_bindings,
        value_bindings,
    )?;

    // make sure the return type and the type of the function body are compatible
    type_env
        .unify(&def.ret_ty, &actual_ret_ty)
        .map_err(|err| Spanned::new(err, def.body.span))?;

    type_bindings.exit_scope();
    value_bindings.exit_scope();
    Ok(())
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Mutability {
    Const,
    Assignment,
    AddrOfMut,
    DerefMut,
    FieldAccess(Box<Mutability>),
}

fn check_expr<'a>(
    expr: Spanned<&'a mut Expr>,
    ret_ty: &TypeRef,
    mutability: Mutability,
    type_env: &mut TypeEnv,
    type_bindings: &mut ScopeMap<Ident, TypeRef>,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<&'a TypeRef, Spanned<TypeError>> {
    let Spanned { value: expr, span } = expr;

    match *expr {
        Expr::Lit(ref lit, ref mut ty) => {
            *ty = match lit {
                Lit::Str(_) => {
                    let str_ty = type_env.insert(Type::Str);
                    type_env.insert(Type::ConstPtr(str_ty))
                }
                Lit::Int(_) => type_env.insert(Type::Int),
                Lit::Bool(_) => type_env.insert(Type::Bool),
            };

            Ok(ty)
        }
        Expr::Var(ref path, ref mut ty) => {
            let binding = value_bindings
                .path_get(path)
                .ok_or_else(|| Spanned::new(TypeError::UndefinedVariable(path.clone()), span))?;

            // if a mut context is required, bindings have to be declared as such
            // mutable derefs are fine, since the pointer must be mut, not the binding
            // for the pointer
            if (mutability != Mutability::Const && mutability != Mutability::DerefMut)
                && !binding.is_mut
            {
                return Err(Spanned::new(TypeError::VarNotMut(mutability), span));
            }

            *ty = binding.ty.clone();
            Ok(ty)
        }
        Expr::UnOp(
            UnOp {
                op,
                ref mut operand,
            },
            ref mut ty,
        ) => {
            let operand = operand.as_mut().map(Box::as_mut);
            let mut new_mutability = Mutability::Const;

            let (expected, expected_expr_ty) = match op {
                UnOpKind::Not => {
                    let ty = type_env.insert(Type::Bool);
                    (ty.clone(), ty)
                }
                UnOpKind::Negate => {
                    let ty = type_env.insert(Type::I32);
                    (ty.clone(), ty)
                }
                UnOpKind::AddrOf => {
                    let rvalue_ty = type_env.insert(Type::Var);
                    let ptr_ty = type_env.insert(Type::ConstPtr(rvalue_ty.clone()));
                    (rvalue_ty, ptr_ty)
                }
                UnOpKind::AddrOfMut => {
                    new_mutability = Mutability::AddrOfMut;
                    let rvalue_ty = type_env.insert(Type::Var);
                    let ptr_ty = type_env.insert(Type::MutPtr(rvalue_ty.clone()));
                    (rvalue_ty, ptr_ty)
                }
                UnOpKind::Deref => {
                    if mutability != Mutability::Const {
                        new_mutability = Mutability::DerefMut;
                    }

                    let pointee_ty = type_env.insert(Type::Var);
                    let ptr_ty = type_env.insert(Type::Ptr(pointee_ty.clone()));
                    (ptr_ty, pointee_ty)
                }
            };

            let actual = check_expr(
                operand,
                ret_ty,
                new_mutability,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            type_env
                .unify(&expected, &actual)
                .map_err(|err| Spanned::new(err, span))?;

            *ty = expected_expr_ty;
            Ok(ty)
        }
        Expr::BinOp(
            BinOp {
                op,
                ref mut lhs,
                ref mut rhs,
            },
            ref mut ty,
        ) => {
            let lhs = lhs.as_mut().map(Box::as_mut);
            let rhs = rhs.as_mut().map(Box::as_mut);

            let (expected, expected_expr_ty) = match op {
                BinOpKind::Or => {
                    let ty = type_env.insert(Type::Bool);
                    (ty.clone(), ty)
                }
                BinOpKind::And => {
                    let ty = type_env.insert(Type::Bool);
                    (ty.clone(), ty)
                }
                BinOpKind::Eq => (type_env.insert(Type::Int), type_env.insert(Type::Bool)),
                BinOpKind::Ne => (type_env.insert(Type::Int), type_env.insert(Type::Bool)),
                BinOpKind::Lt => (type_env.insert(Type::Int), type_env.insert(Type::Bool)),
                BinOpKind::Le => (type_env.insert(Type::Int), type_env.insert(Type::Bool)),
                BinOpKind::Gt => (type_env.insert(Type::Int), type_env.insert(Type::Bool)),
                BinOpKind::Ge => (type_env.insert(Type::Int), type_env.insert(Type::Bool)),
                BinOpKind::Add => {
                    let ty = type_env.insert(Type::Int);
                    (ty.clone(), ty)
                }
                BinOpKind::Sub => {
                    let ty = type_env.insert(Type::Int);
                    (ty.clone(), ty)
                }
                BinOpKind::Mul => {
                    let ty = type_env.insert(Type::Int);
                    (ty.clone(), ty)
                }
                BinOpKind::Div => {
                    let ty = type_env.insert(Type::Int);
                    (ty.clone(), ty)
                }
            };

            let lhs_actual = check_expr(
                lhs,
                ret_ty,
                Mutability::Const,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            let rhs_actual = check_expr(
                rhs,
                ret_ty,
                Mutability::Const,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            // unify lhs, then unify the result with rhs
            let expected = type_env
                .unify(&expected, &lhs_actual)
                .map_err(|err| Spanned::new(err, span))?;

            type_env
                .unify(&expected, &rhs_actual)
                .map_err(|err| Spanned::new(err, span))?;

            *ty = expected_expr_ty;
            Ok(ty)
        }
        Expr::FnCall(ref mut call, ref mut ty) => {
            let binding = value_bindings.path_get(&call.name.value).ok_or_else(|| {
                Spanned::new(TypeError::UndefinedVariable(call.name.value.clone()), span)
            })?;

            let expected = binding.ty.clone();

            // make sure the args are aligned (in case of out of order named args)
            align_args(&binding, call, span)?;

            // check the arguments first and then use their type for the parameters
            let mut params = Vec::with_capacity(call.args.len());

            for arg_expr in call.args.iter_mut().map(|arg| &mut arg.value.value) {
                let arg_ty = check_expr(
                    arg_expr.as_mut(),
                    ret_ty,
                    Mutability::Const,
                    type_env,
                    type_bindings,
                    value_bindings,
                )?;

                params.push(arg_ty.clone());
            }

            // there's no way to know the return type, so just use a variable
            let ret = type_env.insert(Type::Var);

            let fn_ty = type_env.insert(Type::Function(Function {
                params,
                ret: ret.clone(),
            }));

            let actual = type_env.insert(Type::ConstPtr(fn_ty));

            // try to unify with the type of the binding
            type_env
                .unify(&expected, &actual)
                .map_err(|err| Spanned::new(err, span))?;

            // assign the return type as the type of a function call
            *ty = ret;
            Ok(ty)
        }
        Expr::MemberAccess(
            MemberAccess {
                ref mut value,
                ref member,
            },
            ref mut ty,
        ) => {
            // propagate required mutability for a field, since the record holding
            // it must also be mutable
            let new_mutability = if mutability == Mutability::Const {
                Mutability::Const
            } else {
                Mutability::FieldAccess(Box::new(mutability))
            };

            let field_ty = type_env.insert(Type::Var);
            let expected_record = type_env.insert(Type::RecordFields(vec![Field {
                name: member.value.clone(),
                ty: field_ty.clone(),
            }]));

            let actual_ty = check_expr(
                value.as_mut().map(Box::as_mut),
                ret_ty,
                new_mutability,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            // unify the expected record with the actual type of value
            // this also constraints our field type if possible
            type_env
                .unify(&expected_record, &actual_ty)
                .map_err(|err| Spanned::new(err, value.span))?;

            *ty = field_ty;
            Ok(ty)
        }
        Expr::ArrayCons(ref mut cons, ref mut ty) => {
            let elem_ty = type_env.insert(Type::Var);

            // unify all element types with each other
            for elem in &mut cons.elems {
                let ty = check_expr(
                    elem.as_mut(),
                    ret_ty,
                    Mutability::Const,
                    type_env,
                    type_bindings,
                    value_bindings,
                )?;

                type_env
                    .unify(&elem_ty, &ty)
                    .map_err(|err| Spanned::new(err, elem.span))?;
            }

            *ty = type_env.insert(Type::Array(elem_ty, cons.elems.len() as u32));
            Ok(ty)
        }
        Expr::TupleCons(ref mut cons, ref mut ty) => {
            let elem_tys = cons
                .elems
                .iter_mut()
                .map(|elem| {
                    check_expr(
                        elem.as_mut(),
                        ret_ty,
                        Mutability::Const,
                        type_env,
                        type_bindings,
                        value_bindings,
                    )
                    .map(TypeRef::clone)
                })
                .collect::<Result<Vec<_>, _>>()?;

            *ty = type_env.insert(Type::Tuple(elem_tys));
            Ok(ty)
        }
        Expr::Assignment(ref mut assignment, ref mut ty) => {
            // make sure target is an lvalue
            match *assignment.target.value {
                Expr::UnOp(
                    UnOp {
                        op: UnOpKind::Deref,
                        ..
                    },
                    _,
                )
                | Expr::Var(..)
                | Expr::MemberAccess(..) => (),
                _ => {
                    return Err(Spanned::new(
                        TypeError::NonLValueInAssignment,
                        assignment.target.span,
                    ))
                }
            }

            let target_ty = check_expr(
                assignment.target.as_mut().map(Box::as_mut),
                ret_ty,
                Mutability::Assignment,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            let value_ty = check_expr(
                assignment.value.as_mut().map(Box::as_mut),
                ret_ty,
                Mutability::Const,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            type_env
                .unify(&target_ty, &value_ty)
                .map_err(|err| Spanned::new(err, assignment.value.span))?;

            *ty = type_env.insert(Type::unit());
            Ok(ty)
        }
        Expr::LetBinding(
            LetBinding {
                ref pattern,
                ref ty_hint,
                ref mut expr,
            },
            ref mut ty,
        ) => {
            let expr_span = expr.span;

            let expr_ty = check_expr(
                expr.as_mut().map(Box::as_mut),
                ret_ty,
                Mutability::Const,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            // make sure the type and and the expr's type are compatible
            if let Some(ref ty_hint) = ty_hint {
                let hinted_ty = type_from_desc(ty_hint.as_ref(), false, type_env, type_bindings)?;

                type_env
                    .unify(&hinted_ty, &expr_ty)
                    .map_err(|err| Spanned::new(err, expr_span))?;
            }

            // make sure the type is compatible with what can be inferred from the pattern
            let inferred_ty = bind_pattern(&pattern.value, type_env, value_bindings)?;
            type_env
                .unify(&inferred_ty, &expr_ty)
                .map_err(|err| Spanned::new(err, expr.span))?;

            *ty = type_env.insert(Type::unit());
            Ok(ty)
        }
        Expr::AutoRef(ref mut expr, ref mut ty) => {
            // not implemented at the moment, just forward the inner expression
            *ty = check_expr(
                Spanned::new(expr, span),
                ret_ty,
                Mutability::Const,
                type_env,
                type_bindings,
                value_bindings,
            )?
            .clone();

            Ok(ty)
        }
        Expr::Ret(ref mut expr, ref mut ty) => {
            let expr_ty = check_expr(
                expr.as_mut().map(Box::as_mut),
                ret_ty,
                Mutability::Const,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            // make sure the expression's type unifies with the actual return type
            type_env
                .unify(&ret_ty, &expr_ty)
                .map_err(|err| Spanned::new(err, expr.span))?;

            *ty = type_env.insert(Type::Never);
            Ok(ty)
        }
        Expr::IfExpr(ref mut if_expr, ref mut ty) => {
            let cond_ty = check_expr(
                if_expr.cond.as_mut().map(Box::as_mut),
                ret_ty,
                Mutability::Const,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            // the condition must be a bool
            let bool_ty = type_env.insert(Type::Bool);
            type_env
                .unify(&bool_ty, &cond_ty)
                .map_err(|err| Spanned::new(err, if_expr.cond.span))?;

            let then_ty = check_expr(
                if_expr.then_block.as_mut().map(Box::as_mut),
                ret_ty,
                Mutability::Const,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            if let Some(ref mut else_block) = if_expr.else_block {
                let else_ty = check_expr(
                    else_block.as_mut().map(Box::as_mut),
                    ret_ty,
                    Mutability::Const,
                    type_env,
                    type_bindings,
                    value_bindings,
                )?;

                type_env
                    .unify(&then_ty, &else_ty)
                    .map_err(|err| Spanned::new(err, else_block.span))?;
            }

            // use the blocks expression only both then and else exist
            *ty = if if_expr.else_block.is_some() {
                then_ty.clone()
            } else {
                type_env.insert(Type::unit())
            };

            Ok(ty)
        }
        Expr::Block(ref mut block, ref mut ty) => {
            type_bindings.enter_scope();
            value_bindings.enter_scope();
            let mut last_ty = None;

            for expr in &mut block.exprs {
                last_ty = Some(check_expr(
                    expr.as_mut(),
                    ret_ty,
                    Mutability::Const,
                    type_env,
                    type_bindings,
                    value_bindings,
                )?)
                .cloned();
            }

            type_bindings.exit_scope();
            value_bindings.exit_scope();

            let is_last_ret_expr = match block.exprs.last() {
                Some(Spanned {
                    value: Expr::Ret(..),
                    ..
                }) => true,
                _ => false,
            };

            // the type of a block that ends in a statement is unit, unless it's
            // a ret expression, in which case the type of the expression is forwarded.
            // that allows writing something like `ret 1;` which is then equivalent to
            // `ret 1`.
            // If the last expression is not a statement, the expression's type is the type of
            // the block.
            let block_ty = match last_ty {
                Some(ref last_ty) if !block.is_last_expr_stmt || is_last_ret_expr => {
                    last_ty.clone()
                }
                _ => type_env.insert(Type::unit()),
            };

            *ty = block_ty;
            Ok(ty)
        }
    }
}

fn align_args(
    binding: &Binding,
    call: &mut FnCall,
    call_span: Span,
) -> Result<(), Spanned<TypeError>> {
    // swap args out to work on them
    let args = mem::replace(&mut call.args, Vec::new());

    let (named_args, unnamed_args): (Vec<_>, _) =
        args.into_iter().partition(|arg| arg.value.name.is_some());

    // if no names are stored, there cannot be any named args,
    // in which case we can skip the alignment process
    let param_names = match binding.param_names {
        Some(ref param_names) => param_names,
        None => {
            if named_args.is_empty() {
                mem::replace(&mut call.args, unnamed_args);
                return Ok(());
            } else {
                // just use the first arg for the error
                // unwraps are fine since we know it's named arg
                let arg_span = named_args[0].value.name.as_ref().unwrap().span;
                let arg_name = named_args[0].value.name.clone().unwrap().value;
                return Err(Spanned::new(TypeError::UnknownArgName(arg_name), arg_span));
            }
        }
    };

    // lengths of arg and param list must match
    if param_names.len() != (named_args.len() + unnamed_args.len()) {
        return Err(Spanned::new(
            TypeError::ArityMismatch(param_names.len(), named_args.len() + unnamed_args.len()),
            call_span,
        ));
    }

    // store the original order of the params
    let mut param_names: Vec<_> = param_names.iter().enumerate().collect();
    let mut aligned = Vec::with_capacity(param_names.len());

    // collect the named args first
    for arg in named_args {
        let arg_name = arg.value.name.as_ref().map(|spanned| &spanned.value);

        let (idx, _) =
            support::find_remove(&mut param_names, |(_, name)| name.as_ref() == arg_name)
                .ok_or_else(|| {
                    Spanned::new(
                        TypeError::UnknownArgName(arg_name.cloned().unwrap()),
                        arg.value.name.as_ref().unwrap().span,
                    )
                })?;

        aligned.push((idx, arg));
    }

    // the unnamed args simply take the remaining params from left to right
    for (arg, (idx, _)) in unnamed_args.into_iter().zip(param_names) {
        aligned.push((idx, arg));
    }

    // sort by the actual param order
    aligned.sort_unstable_by_key(|&(idx, _)| idx);
    let aligned: Vec<_> = aligned.into_iter().map(|(_, arg)| arg).collect();

    // swap aligned args in
    mem::replace(&mut call.args, aligned);
    Ok(())
}

fn bind_pattern(
    pattern: &Pattern,
    type_env: &mut TypeEnv,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<TypeRef, Spanned<TypeError>> {
    let inferred_ty = match *pattern {
        Pattern::Discard => type_env.insert(Type::Var),
        Pattern::Binding(ref ident) => {
            let ty = type_env.insert(Type::Var);
            value_bindings.insert(ident.value.clone(), Binding::new(ty.clone()));
            ty
        }
        Pattern::MutBinding(ref ident) => {
            let ty = type_env.insert(Type::Var);
            value_bindings.insert(ident.value.clone(), Binding::new_mut(ty.clone()));
            ty
        }
        Pattern::Tuple(ref patterns) => {
            let pattern_tys = patterns
                .value
                .iter()
                .map(|pattern| bind_pattern(pattern, type_env, value_bindings))
                .collect::<Result<Vec<_>, _>>()?;

            type_env.insert(Type::Tuple(pattern_tys))
        }
    };

    Ok(inferred_ty)
}

fn bind_record_def(
    def: &RecordDef,
    type_env: &mut TypeEnv,
    type_bindings: &ScopeMap<Ident, TypeRef>,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<(), Spanned<TypeError>> {
    // the type binding must already have been created by `check_items`
    let current_ty = &type_bindings.get(&def.name.value).unwrap();

    let mut fields = Vec::with_capacity(def.fields.len());
    let mut cons_params = Vec::with_capacity(def.fields.len());
    let mut cons_param_names = Vec::<Option<_>>::with_capacity(def.fields.len());

    for field_def in &def.fields {
        let span = field_def.span;
        let field_def = &field_def.value;

        let name = field_def.name.value.clone();
        let ty = type_from_desc(field_def.ty.as_ref(), true, type_env, type_bindings)?;

        fields.push(Field {
            name: name.clone(),
            ty: ty.clone(),
        });

        // do not allow duplicate field names
        if cons_param_names
            .iter()
            .any(|param| Some(&name) == param.as_ref())
        {
            return Err(Spanned::new(
                TypeError::DuplicatFieldName(name.clone()),
                span,
            ));
        }

        cons_params.push(ty);
        cons_param_names.push(Some(name));
    }

    let record = Record {
        id: TypeId::new(),
        name: def.name.value.clone(),
        fields,
    };

    // type must be a variable right now, since we are creating the actual type
    let ty = type_env.insert(Type::Record(record));
    type_env.unify(&current_ty, &ty).unwrap();

    let cons = Function {
        params: cons_params,
        ret: ty,
    };

    let cons_fn = type_env.insert(Type::Function(cons));
    let cons_ty = type_env.insert(Type::ConstPtr(cons_fn));

    // bind the type constructor
    value_bindings.insert(
        def.name.value.clone(),
        Binding::new_fn(cons_ty, cons_param_names),
    );

    Ok(())
}

fn bind_variants_def(
    def: &VariantsDef,
    type_env: &mut TypeEnv,
    type_bindings: &ScopeMap<Ident, TypeRef>,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<(), Spanned<TypeError>> {
    // the type binding must already have been created by `check_items`
    let current_ty = type_bindings.get(&def.name.value).unwrap();

    let mut variants = Vec::with_capacity(def.variants.len());

    for variant_def in &def.variants {
        let variant_def = &variant_def.value;

        let params = variant_def
            .param_tys
            .iter()
            .map(|param_ty| type_from_desc(param_ty.as_ref(), true, type_env, type_bindings))
            .collect::<Result<Vec<_>, _>>()?;

        variants.push(Variant {
            name: variant_def.name.value.clone(),
            params: params.clone(),
        });

        // if the params are empty, the variant is a constant value, otherwise
        // it's as constructor function
        let variant_cons_ty = if params.is_empty() {
            current_ty.clone()
        } else {
            let cons = Function {
                params,
                ret: current_ty.clone(),
            };
            let cons_fn = type_env.insert(Type::Function(cons));
            type_env.insert(Type::ConstPtr(cons_fn))
        };

        // bind the variant constructor scoped under the type name
        value_bindings.path_insert(
            vec![def.name.value.clone(), variant_def.name.value.clone()],
            Binding::new(variant_cons_ty),
        );
    }

    let variants = Variants {
        id: TypeId::new(),
        name: def.name.value.clone(),
        variants,
    };

    // type must be a variable right now, since we are creating the actual type
    let ty = type_env.insert(Type::Variants(variants));
    type_env.unify(&current_ty, &ty).unwrap();

    Ok(())
}

fn type_from_desc(
    desc: Spanned<&TypeDesc>,
    is_type_def: bool,
    type_env: &mut TypeEnv,
    type_bindings: &ScopeMap<Ident, TypeRef>,
) -> Result<TypeRef, Spanned<TypeError>> {
    let Spanned { value: desc, span } = desc;

    let ty = match *desc {
        TypeDesc::Hole => {
            if is_type_def {
                return Err(Spanned::new(TypeError::HoleInTypeDef, span));
            } else {
                type_env.insert(Type::Var)
            }
        }
        TypeDesc::Name(ref ident) => type_bindings
            .get(ident)
            .ok_or_else(|| {
                Spanned::new(
                    TypeError::UndefinedType(ItemPath::from(Spanned::new(ident.clone(), span))),
                    span,
                )
            })?
            .clone(),
        TypeDesc::ConstPtr(ref desc) => {
            let inner_ty = type_from_desc(
                desc.as_ref().map(Rc::as_ref),
                is_type_def,
                type_env,
                type_bindings,
            )?;

            type_env.insert(Type::ConstPtr(inner_ty))
        }
        TypeDesc::MutPtr(ref desc) => {
            let inner_ty = type_from_desc(
                desc.as_ref().map(Rc::as_ref),
                is_type_def,
                type_env,
                type_bindings,
            )?;

            type_env.insert(Type::MutPtr(inner_ty))
        }
        TypeDesc::Array(ArrayDesc { ref ty, ref len }) => {
            let elem_ty = type_from_desc(
                ty.as_ref().map(Rc::as_ref),
                is_type_def,
                type_env,
                type_bindings,
            )?;

            type_env.insert(Type::Array(elem_ty, len.into_inner()))
        }
        TypeDesc::Function(FunctionDesc {
            ref param_tys,
            ref ret_ty,
        }) => {
            let params = param_tys
                .iter()
                .map(|param_desc| {
                    type_from_desc(
                        param_desc.as_ref().map(Rc::as_ref),
                        is_type_def,
                        type_env,
                        type_bindings,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            let ret = type_from_desc(
                ret_ty.as_ref().map(Rc::as_ref),
                is_type_def,
                type_env,
                type_bindings,
            )?;

            let fn_ty = type_env.insert(Type::Function(Function { params, ret }));
            type_env.insert(Type::ConstPtr(fn_ty))
        }
        TypeDesc::Tuple(ref ty_descs) => {
            let tys = ty_descs
                .iter()
                .map(|ty_desc| {
                    type_from_desc(
                        ty_desc.as_ref().map(Rc::as_ref),
                        is_type_def,
                        type_env,
                        type_bindings,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            type_env.insert(Type::Tuple(tys))
        }
    };

    Ok(ty)
}

fn bind_primitives(type_env: &mut TypeEnv, type_bindings: &mut ScopeMap<Ident, TypeRef>) {
    type_bindings.insert(Ident::new("bool"), type_env.insert(Type::Bool));
    type_bindings.insert(Ident::new("i32"), type_env.insert(Type::I32));
    type_bindings.insert(Ident::new("u32"), type_env.insert(Type::U32));
    type_bindings.insert(Ident::new("str"), type_env.insert(Type::Str));
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer;
    use crate::parser;

    #[test]
    fn check_syntax_example() {
        let tokens = lexer::lex(include_str!("../tests/syntax.cml")).unwrap();
        let ast = parser::parse(&tokens).unwrap();
        typecheck(ast).unwrap();
    }
}
