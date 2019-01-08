pub mod scope_map;
pub mod unify;

use crate::ast::*;
use crate::span::{Span, Spanned};
use crate::support;
use crate::typecheck::scope_map::ScopeMap;
use crate::typecheck::unify::TypeEnv;
use std::mem;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug)]
pub enum TypeError {
    HoleInTypeDef,
    UndefinedType(ItemPath),
    UndefinedVariable(ItemPath),
    DuplicatParamName(Ident),
    DuplicatFieldName(Ident),
    VarNotMut(Mutability),
    UnknownArgName(Ident),
    ArityMismatch(usize, usize),
    Mismatch(Type, Type),
}

#[derive(Debug, Clone, Copy)]
pub struct TypeRef(usize);

impl TypeRef {
    pub fn invalid() -> TypeRef {
        TypeRef(usize::max_value())
    }
}

#[derive(Debug)]
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
    Record(Record),
    Variants(Variants),
}

impl Type {
    pub fn unit() -> Type {
        Type::Tuple(Vec::new())
    }
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<TypeRef>,
    pub ret: TypeRef,
}

#[derive(Debug)]
pub struct Record {
    pub id: TypeId,
    pub fields: Vec<Field>,
}

impl PartialEq for Record {
    fn eq(&self, rhs: &Record) -> bool {
        self.id == rhs.id
    }
}

#[derive(Debug)]
pub struct Field {
    pub name: Ident,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct Variants {
    pub id: TypeId,
    pub variants: Vec<Variant>,
}

impl PartialEq for Variants {
    fn eq(&self, rhs: &Variants) -> bool {
        self.id == rhs.id
    }
}

#[derive(Debug)]
pub struct Variant {
    pub name: Ident,
    pub params: Vec<TypeRef>,
}

#[derive(Debug, PartialEq, Eq)]
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

    fn ty(&self) -> TypeRef {
        self.ty
    }
}

pub fn typecheck(mut ast: Ast) -> Result<Ast, Spanned<TypeError>> {
    let mut type_bindings = ScopeMap::new();
    bind_primitives(&mut ast.type_env, &mut type_bindings);
    let mut value_bindings = ScopeMap::new();

    check_items(
        &mut ast.items,
        &mut ast.type_env,
        &mut type_bindings,
        &mut value_bindings,
    )?;

    // TODO: verify generated types
    Ok(ast)
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
                type_bindings.insert(name.value.clone(), type_env.new_type_var());
            }
            Item::FnDef(FnDef {
                ref name,
                ref params,
                ..
            }) => {
                let ty = type_env.new_type_var();

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
                let var = *type_bindings.get(&name.value).unwrap();
                type_env.unify(var, ty).unwrap();
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
    let current_ty = value_bindings.get(&def.name.value).unwrap().ty;

    type_bindings.enter_scope();
    value_bindings.enter_scope();

    let mut params = Vec::with_capacity(def.params.len());

    for param in &mut def.params {
        let param = &mut param.value;

        param.ty = match param.ty_hint.as_ref() {
            Some(hint) => type_from_desc(hint.as_ref(), false, type_env, type_bindings)?,
            None => type_env.new_type_var(),
        };

        // bind the parameter if it has a name
        if let Some(ref name) = &param.name.value {
            value_bindings.insert(name.clone(), Binding::new(param.ty));
        }

        params.push(param.ty);
    }

    def.ret_ty = match def.ret_ty_hint.as_ref() {
        Some(ty) => type_from_desc(ty.as_ref(), false, type_env, type_bindings)?,
        None => type_env.new_type_var(),
    };

    // update the functions type
    // since it was just a variable before, unification cannot fail
    let fn_ty = type_env.insert(Type::Function(Function {
        params,
        ret: def.ret_ty,
    }));

    let ty = type_env.insert(Type::ConstPtr(fn_ty));
    type_env.unify(current_ty, ty).unwrap();

    // check the function body
    check_expr(
        def.body.as_mut(),
        def.ret_ty,
        Mutability::Const,
        type_env,
        type_bindings,
        value_bindings,
    )?;

    Ok(())
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mutability {
    Const,
    Assignment,
    AddrOfMut,
    DerefMut,
}

fn check_expr(
    expr: Spanned<&mut Expr>,
    ret_ty: TypeRef,
    mutability: Mutability,
    type_env: &mut TypeEnv,
    type_bindings: &mut ScopeMap<Ident, TypeRef>,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<TypeRef, Spanned<TypeError>> {
    // almost no expression requires a mut context for its nested expression;
    // the exceptions have to set this accordingly
    let mut nested_mutability = Mutability::Const;
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

            Ok(*ty)
        }
        Expr::Var(ref path, ref mut ty) => {
            let binding = value_bindings
                .path_get(path)
                .ok_or_else(|| Spanned::new(TypeError::UndefinedVariable(path.clone()), span))?;

            // if a mut context is required, bindings have to be declared as such
            if mutability != Mutability::Const && !binding.is_mut {
                return Err(Spanned::new(TypeError::VarNotMut(mutability), span));
            }

            *ty = binding.ty;
            Ok(*ty)
        }
        Expr::UnOp(
            UnOp {
                op,
                ref mut operand,
            },
            ref mut ty,
        ) => {
            let operand = operand.as_mut().map(Box::as_mut);

            let expected = match op {
                UnOpKind::Not => type_env.insert(Type::Bool),
                UnOpKind::Negate => type_env.insert(Type::I32),
                UnOpKind::AddrOf => type_env.insert(Type::Var),
                UnOpKind::AddrOfMut => {
                    nested_mutability = Mutability::AddrOfMut;
                    type_env.insert(Type::Var)
                }
                UnOpKind::Deref => {
                    if mutability != Mutability::Const {
                        nested_mutability = Mutability::DerefMut;
                    }

                    let pointee_ty = type_env.insert(Type::Var);
                    type_env.insert(Type::Ptr(pointee_ty))
                }
            };

            let actual = check_expr(
                operand,
                ret_ty,
                nested_mutability,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            *ty = type_env
                .unify(expected, actual)
                .map_err(|err| Spanned::new(err, span))?;

            Ok(*ty)
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

            let expected = match op {
                BinOpKind::Or => type_env.insert(Type::Bool),
                BinOpKind::And => type_env.insert(Type::Bool),
                BinOpKind::Eq => type_env.insert(Type::Int),
                BinOpKind::Ne => type_env.insert(Type::Int),
                BinOpKind::Lt => type_env.insert(Type::Int),
                BinOpKind::Le => type_env.insert(Type::Int),
                BinOpKind::Gt => type_env.insert(Type::Int),
                BinOpKind::Ge => type_env.insert(Type::Int),
                BinOpKind::Add => type_env.insert(Type::Int),
                BinOpKind::Sub => type_env.insert(Type::Int),
                BinOpKind::Mul => type_env.insert(Type::Int),
                BinOpKind::Div => type_env.insert(Type::Int),
            };

            let lhs_actual = check_expr(
                lhs,
                ret_ty,
                nested_mutability,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            let rhs_actual = check_expr(
                rhs,
                ret_ty,
                nested_mutability,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            // unify lhs, then unify the result with rhs
            let expected = type_env
                .unify(expected, lhs_actual)
                .map_err(|err| Spanned::new(err, span))?;

            *ty = type_env
                .unify(expected, rhs_actual)
                .map_err(|err| Spanned::new(err, span))?;

            Ok(*ty)
        }
        Expr::FnCall(ref mut call, ref mut ty) => {
            let binding = value_bindings.path_get(&call.name.value).ok_or_else(|| {
                Spanned::new(TypeError::UndefinedVariable(call.name.value.clone()), span)
            })?;

            let expected = binding.ty;

            // make sure the args are aligned (in case of out of order named args)
            align_args(&binding, call, span)?;

            // check the arguments first and then use their type for the parameters
            let mut params = Vec::with_capacity(call.args.len());

            for arg_expr in call.args.iter_mut().map(|arg| &mut arg.value.value) {
                let arg_ty = check_expr(
                    arg_expr.as_mut(),
                    ret_ty,
                    nested_mutability,
                    type_env,
                    type_bindings,
                    value_bindings,
                )?;

                params.push(arg_ty);
            }

            // there's no way to know the return type, so just use a variable
            let ret = type_env.insert(Type::Var);
            let actual = type_env.insert(Type::Function(Function { params, ret }));

            // try to unify with the type of the binding
            *ty = type_env
                .unify(expected, actual)
                .map_err(|err| Spanned::new(err, span))?;

            Ok(*ty)
        }
        Expr::Block(ref mut block, ref mut ty) => {
            let mut last_ty = None;

            for expr in &mut block.exprs {
                last_ty = Some(check_expr(
                    expr.as_mut(),
                    ret_ty,
                    nested_mutability,
                    type_env,
                    type_bindings,
                    value_bindings,
                )?);
            }

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
                Some(last_ty) if !block.is_last_expr_stmt || is_last_ret_expr => last_ty,
                _ => type_env.insert(Type::unit()),
            };

            *ty = block_ty;
            Ok(*ty)
        }
        // TODO: mutability for member access
        _ => unimplemented!(),
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

fn bind_record_def(
    def: &RecordDef,
    type_env: &mut TypeEnv,
    type_bindings: &ScopeMap<Ident, TypeRef>,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<(), Spanned<TypeError>> {
    // the type binding must already have been created by `check_items`
    let current_ty = *type_bindings.get(&def.name.value).unwrap();

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
            ty,
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
        fields,
    };

    // type must be a variable right now, since we are creating the actual type
    let ty = type_env.insert(Type::Record(record));
    type_env.unify(current_ty, ty).unwrap();

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
    let current_ty = *type_bindings.get(&def.name.value).unwrap();

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

        let variant_cons = Function {
            params,
            ret: current_ty,
        };

        // bind the variant constructor as function scoped under the type name
        let cons_fn = type_env.insert(Type::Function(variant_cons));
        let variant_cons_ty = type_env.insert(Type::ConstPtr(cons_fn));

        value_bindings.path_insert(
            vec![def.name.value.clone(), variant_def.name.value.clone()],
            Binding::new(variant_cons_ty),
        );
    }

    let variants = Variants {
        id: TypeId::new(),
        variants,
    };

    // type must be a variable right now, since we are creating the actual type
    let ty = type_env.insert(Type::Variants(variants));
    type_env.unify(current_ty, ty).unwrap();

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
                type_env.new_type_var()
            }
        }
        TypeDesc::Name(ref ident) => *type_bindings.get(ident).ok_or_else(|| {
            Spanned::new(
                TypeError::UndefinedType(ItemPath::from(Spanned::new(ident.clone(), span))),
                span,
            )
        })?,
        TypeDesc::ConstPtr(ref desc) => {
            let inner_ty = type_from_desc(
                desc.as_ref().map(Box::as_ref),
                is_type_def,
                type_env,
                type_bindings,
            )?;

            type_env.insert(Type::ConstPtr(inner_ty))
        }
        TypeDesc::MutPtr(ref desc) => {
            let inner_ty = type_from_desc(
                desc.as_ref().map(Box::as_ref),
                is_type_def,
                type_env,
                type_bindings,
            )?;

            type_env.insert(Type::MutPtr(inner_ty))
        }
        TypeDesc::Array(ArrayDesc { ref ty, ref len }) => {
            let elem_ty = type_from_desc(
                ty.as_ref().map(Box::as_ref),
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
                    type_from_desc(param_desc.as_ref(), is_type_def, type_env, type_bindings)
                })
                .collect::<Result<Vec<_>, _>>()?;

            let ret = type_from_desc(
                ret_ty.as_ref().map(Box::as_ref),
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
                    type_from_desc(ty_desc.as_ref(), is_type_def, type_env, type_bindings)
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
