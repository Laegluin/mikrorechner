pub mod lint;
pub mod unify;

use crate::ast::*;
use crate::codegen::ENTRY_POINT;
use crate::scope_map::ScopeMap;
use crate::span::{Span, Spanned};
use crate::support;
use crate::typecheck::unify::TypeEnv;
use std::fmt::{self, Display};
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
    Mismatch(Rc<TypeDesc>, Rc<TypeDesc>),
    NonLValueInAssignment,
    CannotInfer(Rc<TypeDesc>),
    TypeIsNotCastable(Rc<TypeDesc>),
    EntryPointTypeMismatch(Rc<TypeDesc>),
    ShadowedFnDef,
    ShadowedTypeDef,
}

impl Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TypeError::*;

        match *self {
            HoleInTypeDef => write!(f, "wildcard types are not allowed in type definitions"),
            UndefinedType(ref path) => write!(f, "undefined type `{}`", path),
            UndefinedVariable(ref path) => write!(f, "undefined variable `{}`", path),
            DuplicatParamName(ref name) => write!(f, "duplicate parameter `{}`", name),
            DuplicatFieldName(ref name) => write!(f, "duplicate field `{}`", name),
            UnknownFieldName(ref name) => write!(f, "record does not have field `{}`", name),
            VarNotMut(ref mutability) => {
                write!(f, "variable is not mutable ")?;

                match mutability {
                    Mutability::Const => unreachable!(),
                    Mutability::Assignment => write!(f, "(required for assignment)"),
                    Mutability::AddrOfMut => write!(f, "(required for mutable address of)"),
                    Mutability::DerefMut => write!(f, "(required for mutable deref)"),
                    Mutability::FieldAccess(_) => write!(f, "(required for mutable field access)"),
                }
            }
            UnknownArgName(ref name) => write!(
                f,
                "function binding does not have named parameter `{}`",
                name
            ),
            ArityMismatch(expected, actual) => write!(
                f,
                "incorrect argument count (expected {}, found {})",
                expected, actual
            ),
            Mismatch(ref expected, ref actual) => write!(
                f,
                "type mismatch: expected `{}`, found `{}`",
                expected, actual
            ),
            NonLValueInAssignment => write!(
                f,
                "left hand side of assignment must be a variable or a deref expression"
            ),
            CannotInfer(ref ty) => write!(f, "cannot infer type for `{}`", ty),
            TypeIsNotCastable(ref ty) => write!(f, "cannot cast type `{}`", ty),
            EntryPointTypeMismatch(ref ty) => write!(
                f,
                "{} must be of type `*fn -> ()`, found `{}`",
                ENTRY_POINT, ty
            ),
            ShadowedFnDef => write!(f, "function definition shadows previous definition"),
            ShadowedTypeDef => write!(f, "type definition shadows previous definition"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeRef(usize, Rc<TypeDesc>);

impl TypeRef {
    fn new(id: usize, ty: &Type) -> TypeRef {
        TypeRef(id, Rc::new(TypeDesc::from_type(ty)))
    }

    /// Changes the type description without changing the type the `TypeRef` it refers to.
    fn with_desc(self, ty: &Type) -> TypeRef {
        TypeRef::new(self.0, ty)
    }

    pub fn desc(&self) -> Rc<TypeDesc> {
        Rc::clone(&self.1)
    }

    pub fn invalid() -> TypeRef {
        TypeRef(
            usize::max_value(),
            Rc::new(TypeDesc::Name(Ident::new("{invalid}"))),
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

impl Display for TypeRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.1.fmt(f)
    }
}

#[derive(Debug)]
pub enum TypeDesc {
    Hole,
    Name(Ident),
    ConstPtr(Rc<TypeDesc>),
    MutPtr(Rc<TypeDesc>),
    Array(Rc<TypeDesc>, u32),
    Function(Vec<Rc<TypeDesc>>, Rc<TypeDesc>),
    Tuple(Vec<Rc<TypeDesc>>),
    PartialRecord(Vec<(Ident, Rc<TypeDesc>)>),
}

impl TypeDesc {
    pub fn from_type(ty: &Type) -> TypeDesc {
        match *ty {
            Type::Var => TypeDesc::Hole,
            Type::Int => TypeDesc::Name(Ident::new("{integer}")),
            Type::Never => TypeDesc::Name(Ident::new("!")),
            Type::Bool => TypeDesc::Name(Ident::new("bool")),
            Type::I32 => TypeDesc::Name(Ident::new("i32")),
            Type::U32 => TypeDesc::Name(Ident::new("u32")),
            Type::Str => TypeDesc::Name(Ident::new("str")),
            Type::Ptr(ref ty_ref) => TypeDesc::ConstPtr(ty_ref.desc()),
            Type::ConstPtr(ref ty_ref) => TypeDesc::ConstPtr(ty_ref.desc()),
            Type::MutPtr(ref ty_ref) => TypeDesc::MutPtr(ty_ref.desc()),
            Type::Array(ref ty_ref, len) => TypeDesc::Array(ty_ref.desc(), len),
            Type::Tuple(ref ty_refs) => {
                TypeDesc::Tuple(ty_refs.iter().map(|ty_ref| ty_ref.desc()).collect())
            }
            Type::Function(ref func) => TypeDesc::Function(
                func.params.iter().map(|ty_ref| ty_ref.desc()).collect(),
                func.ret.desc(),
            ),
            Type::PartialRecord(ref fields) => TypeDesc::PartialRecord(
                fields
                    .iter()
                    .map(|field| (field.name.clone(), field.ty.desc()))
                    .collect(),
            ),
            Type::Record(ref record) => TypeDesc::Name(record.name.clone()),
        }
    }
}

impl Display for TypeDesc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TypeDesc::Hole => write!(f, "_"),
            TypeDesc::Name(ref ident) => write!(f, "{}", ident),
            TypeDesc::ConstPtr(ref inner) => write!(f, "*{}", inner),
            TypeDesc::MutPtr(ref inner) => write!(f, "*mut {}", inner),
            TypeDesc::Array(ref inner, len) => write!(f, "[{}; {}]", inner, len),
            TypeDesc::Tuple(ref elems) => {
                write!(f, "(")?;

                let elems_str = elems
                    .iter()
                    .map(|desc| desc.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "{}", elems_str)?;

                if elems.len() == 1 {
                    write!(f, ",")?;
                }

                write!(f, ")")
            }
            TypeDesc::Function(ref params, ref ret) => {
                if params.is_empty() {
                    return write!(f, "fn -> {}", ret);
                }

                write!(f, "fn ")?;

                let params_str = params
                    .iter()
                    .map(|desc| desc.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "{} -> {}", params_str, ret)
            }
            TypeDesc::PartialRecord(ref fields) => {
                if fields.is_empty() {
                    return write!(f, "{{ .. }}");
                }

                write!(f, "{{ ")?;

                let fields_str = fields
                    .iter()
                    .map(|(name, desc)| format!("{}: {}", name, desc))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "{}", fields_str)?;
                write!(f, ", .. }}")
            }
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
    PartialRecord(Vec<Field>),
    Record(Record),
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
    items: &mut Vec<Spanned<Item>>,
    type_env: &mut TypeEnv,
    type_bindings: &mut ScopeMap<Ident, TypeRef>,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<(), Spanned<TypeError>> {
    // collect all type and function defs, but delay resolution to allow for (mutally) recursive definitions
    for item in &*items {
        match item.value {
            Item::TypeDef(TypeDef::Alias(Spanned {
                value: AliasDef { ref name, .. },
                ..
            }))
            | Item::TypeDef(TypeDef::RecordDef(Spanned {
                value: RecordDef { ref name, .. },
                ..
            })) => {
                let prev_binding =
                    type_bindings.insert(name.value.clone(), type_env.insert(Type::Var));

                if prev_binding.is_some() {
                    return Err(Spanned::new(TypeError::ShadowedTypeDef, name.span));
                }
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

                let prev_binding =
                    value_bindings.path_insert(name.clone(), Binding::new_fn(ty, param_names));

                if prev_binding.is_some() {
                    return Err(Spanned::new(TypeError::ShadowedFnDef, name.span()));
                }
            }
        }
    }

    let mut constructors = Vec::new();

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
                let ty = type_from_decl(ty.as_ref(), true, type_env, type_bindings)?;
                let var = &type_bindings.get(&name.value).unwrap();
                type_env.unify(&var, &ty).unwrap();
            }
            Item::TypeDef(TypeDef::RecordDef(Spanned {
                value: ref def,
                span,
            })) => {
                let cons = bind_record_def(def, type_env, type_bindings, value_bindings)?;
                constructors.push(Spanned::new(Item::FnDef(cons), span));
            }
            Item::FnDef(_) => (),
        }
    }

    // add the constructors to the items
    items.append(&mut constructors);

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
    let current_ty = value_bindings.path_get(&def.name).unwrap().ty.clone();

    type_bindings.enter_scope();
    value_bindings.enter_scope();

    let mut params = Vec::with_capacity(def.params.len());

    for param in &mut def.params {
        let param = &mut param.value;

        param.ty = match param.ty_hint.as_ref() {
            Some(hint) => type_from_decl(hint.as_ref(), false, type_env, type_bindings)?,
            None => type_env.insert(Type::Var),
        };

        // bind the parameter if it has a name
        if let Some(ref name) = &param.name.value {
            value_bindings.insert(name.clone(), Binding::new(param.ty.clone()));
        }

        params.push(param.ty.clone());
    }

    def.ret_ty = match def.ret_ty_hint.as_ref() {
        Some(ty) => type_from_decl(ty.as_ref(), false, type_env, type_bindings)?,
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
    )?
    .clone();

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
    ret_ty: &'a TypeRef,
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
        Expr::Cast(
            Cast {
                ref mut expr,
                ref to_ty,
            },
            ref mut ty,
        ) => {
            let expr_ty = check_expr(
                expr.as_mut().map(Box::as_mut),
                ret_ty,
                Mutability::Const,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            ensure_is_castable(expr_ty, type_env).map_err(|err| Spanned::new(err, expr.span))?;
            let cast_ty = type_from_decl(to_ty.as_ref(), false, type_env, type_bindings)?;
            ensure_is_castable(&cast_ty, type_env).map_err(|err| Spanned::new(err, to_ty.span))?;

            *ty = cast_ty;
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
            let expected_record = type_env.insert(Type::PartialRecord(vec![Field {
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
                    ));
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
                ref mut pattern,
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
                let hinted_ty = type_from_decl(ty_hint.as_ref(), false, type_env, type_bindings)?;

                type_env
                    .unify(&hinted_ty, &expr_ty)
                    .map_err(|err| Spanned::new(err, expr_span))?;
            }

            // make sure the type is compatible with what can be inferred from the pattern
            let inferred_ty = bind_pattern(&mut pattern.value, type_env, value_bindings);
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
            let expr_ty = match expr {
                Some(expr) => check_expr(
                    expr.as_mut().map(Box::as_mut),
                    ret_ty,
                    Mutability::Const,
                    type_env,
                    type_bindings,
                    value_bindings,
                )?
                .clone(),
                None => type_env.insert(Type::unit()),
            };

            // make sure the expression's type unifies with the actual return type
            type_env
                .unify(&ret_ty, &expr_ty)
                .map_err(|err| Spanned::new(err, expr.as_ref().map_or(span, |expr| expr.span)))?;

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
        Expr::WhileExpr(ref mut while_expr, ref mut ty) => {
            let cond_ty = check_expr(
                while_expr.cond.as_mut().map(Box::as_mut),
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
                .map_err(|err| Spanned::new(err, while_expr.cond.span))?;

            let body_ty = check_expr(
                while_expr.body.as_mut().map(Box::as_mut),
                ret_ty,
                Mutability::Const,
                type_env,
                type_bindings,
                value_bindings,
            )?;

            // body must be a ()
            let unit_ty = type_env.insert(Type::unit());
            type_env
                .unify(&unit_ty, body_ty)
                .map_err(|err| Spanned::new(err, while_expr.body.span))?;

            *ty = unit_ty;
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
        Expr::ConstructRecord(ref mut ty) => {
            // we know the type of a constructor is simply the return type of the constructor function
            *ty = ret_ty.clone();
            Ok(ret_ty)
        }
    }
}

fn ensure_is_castable(ty: &TypeRef, type_env: &TypeEnv) -> Result<(), TypeError> {
    let (ty_ref, ty) = type_env.find_type(ty);

    match *ty {
        Type::Int | Type::I32 | Type::U32 | Type::Ptr(_) | Type::ConstPtr(_) | Type::MutPtr(_) => {
            Ok(())
        }
        _ => Err(TypeError::TypeIsNotCastable(ty_ref.desc())),
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

fn bind_pattern<'a>(
    pattern: &'a mut Pattern,
    type_env: &mut TypeEnv,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> &'a TypeRef {
    match *pattern {
        Pattern::Discard(ref mut ty) => {
            *ty = type_env.insert(Type::Var);
            ty
        }
        Pattern::Binding(ref ident, ref mut ty) => {
            *ty = type_env.insert(Type::Var);
            value_bindings.insert(ident.value.clone(), Binding::new(ty.clone()));
            ty
        }
        Pattern::MutBinding(ref ident, ref mut ty) => {
            *ty = type_env.insert(Type::Var);
            value_bindings.insert(ident.value.clone(), Binding::new_mut(ty.clone()));
            ty
        }
        Pattern::Tuple(ref mut patterns, ref mut ty) => {
            let pattern_tys = patterns
                .iter_mut()
                .map(|pattern| bind_pattern(&mut pattern.value, type_env, value_bindings).clone())
                .collect::<Vec<_>>();

            *ty = type_env.insert(Type::Tuple(pattern_tys));
            ty
        }
    }
}

fn bind_record_def(
    def: &RecordDef,
    type_env: &mut TypeEnv,
    type_bindings: &ScopeMap<Ident, TypeRef>,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<FnDef, Spanned<TypeError>> {
    // the type binding must already have been created by `check_items`
    let current_ty = &type_bindings.get(&def.name.value).unwrap();

    let mut fields = Vec::with_capacity(def.fields.len());
    let mut cons_params = Vec::with_capacity(def.fields.len());
    let mut cons_param_names = Vec::<Option<_>>::with_capacity(def.fields.len());
    let mut def_params = Vec::with_capacity(def.fields.len());

    for field_def in &def.fields {
        let span = field_def.span;
        let field_def = &field_def.value;

        let name = field_def.name.value.clone();
        let ty = type_from_decl(field_def.ty.as_ref(), true, type_env, type_bindings)?;

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

        cons_params.push(ty.clone());
        cons_param_names.push(Some(name));

        def_params.push(Spanned::new(
            ParamDef::record_cons_param(field_def, ty),
            span,
        ));
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
        ret: ty.clone(),
    };

    let cons_fn = type_env.insert(Type::Function(cons));
    let cons_ty = type_env.insert(Type::ConstPtr(cons_fn));

    // bind the type constructor
    value_bindings.insert(
        def.name.value.clone(),
        Binding::new_fn(cons_ty, cons_param_names),
    );

    Ok(FnDef {
        name: ItemPath::from(def.name.clone()),
        params: def_params,
        ret_ty_hint: Some(def.name.clone().map(TypeDecl::Name)),
        ret_ty: ty,
        body: Spanned::new(Expr::construct_record(), def.name.span),
    })
}

fn type_from_decl(
    desc: Spanned<&TypeDecl>,
    is_type_def: bool,
    type_env: &mut TypeEnv,
    type_bindings: &ScopeMap<Ident, TypeRef>,
) -> Result<TypeRef, Spanned<TypeError>> {
    let Spanned { value: desc, span } = desc;

    let ty = match *desc {
        TypeDecl::Hole => {
            if is_type_def {
                return Err(Spanned::new(TypeError::HoleInTypeDef, span));
            } else {
                type_env.insert(Type::Var)
            }
        }
        TypeDecl::Name(ref ident) => type_bindings
            .get(ident)
            .ok_or_else(|| {
                Spanned::new(
                    TypeError::UndefinedType(ItemPath::from(Spanned::new(ident.clone(), span))),
                    span,
                )
            })?
            .clone(),
        TypeDecl::ConstPtr(ref desc) => {
            let inner_ty = type_from_decl(
                desc.as_ref().map(Box::as_ref),
                is_type_def,
                type_env,
                type_bindings,
            )?;

            type_env.insert(Type::ConstPtr(inner_ty))
        }
        TypeDecl::MutPtr(ref desc) => {
            let inner_ty = type_from_decl(
                desc.as_ref().map(Box::as_ref),
                is_type_def,
                type_env,
                type_bindings,
            )?;

            type_env.insert(Type::MutPtr(inner_ty))
        }
        TypeDecl::Array(ArrayDecl { ref ty, ref len }) => {
            let elem_ty = type_from_decl(
                ty.as_ref().map(Box::as_ref),
                is_type_def,
                type_env,
                type_bindings,
            )?;

            type_env.insert(Type::Array(elem_ty, len.into_inner()))
        }
        TypeDecl::Function(FunctionDecl {
            ref param_tys,
            ref ret_ty,
        }) => {
            let params = param_tys
                .iter()
                .map(|param_desc| {
                    type_from_decl(param_desc.as_ref(), is_type_def, type_env, type_bindings)
                })
                .collect::<Result<Vec<_>, _>>()?;

            let ret = type_from_decl(
                ret_ty.as_ref().map(Box::as_ref),
                is_type_def,
                type_env,
                type_bindings,
            )?;

            let fn_ty = type_env.insert(Type::Function(Function { params, ret }));
            type_env.insert(Type::ConstPtr(fn_ty))
        }
        TypeDecl::Tuple(ref ty_descs) => {
            let tys = ty_descs
                .iter()
                .map(|ty_desc| {
                    type_from_decl(ty_desc.as_ref(), is_type_def, type_env, type_bindings)
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

    #[test]
    fn check_factorial_example() {
        let tokens = lexer::lex(include_str!("../tests/factorial.cml")).unwrap();
        let ast = parser::parse(&tokens).unwrap();
        typecheck(ast).unwrap();
    }

    #[test]
    fn check_pow_example() {
        let tokens = lexer::lex(include_str!("../tests/pow.cml")).unwrap();
        let ast = parser::parse(&tokens).unwrap();
        typecheck(ast).unwrap();
    }

    #[test]
    fn check_vector_example() {
        let tokens = lexer::lex(include_str!("../tests/vector.cml")).unwrap();
        let ast = parser::parse(&tokens).unwrap();
        typecheck(ast).unwrap();
    }
}
