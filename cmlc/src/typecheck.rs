pub mod scope_map;
pub mod unify;

use crate::ast::*;
use crate::span::Spanned;
use crate::typecheck::scope_map::ScopeMap;
use crate::typecheck::unify::TypeEnv;
use fnv::FnvHashSet;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug)]
pub enum TypeError {
    HoleInTypeDef,
    UndefinedType(ItemPath),
    UndefinedVariable(ItemPath),
    DuplicatParamName(Ident),
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
    /// A call to a function. This type is required to handle function calls
    /// with or without named arguments. All instances of this type will be removed during typechecking.
    Call(Function),
    Function(Function),
    Record(Record),
    Variants(Variants),
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<Param>,
    pub ret: TypeRef,
}

#[derive(Debug)]
pub struct Param {
    pub name: Option<Ident>,
    pub ty: TypeRef,
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
}

impl Binding {
    fn new(ty: TypeRef) -> Binding {
        Binding { ty, is_mut: false }
    }

    fn new_mut(ty: TypeRef) -> Binding {
        Binding { ty, is_mut: true }
    }
}

pub fn typecheck(mut ast: Ast) -> Result<Ast, Spanned<TypeError>> {
    let mut type_bindings = ScopeMap::new();
    let mut value_bindings = ScopeMap::new();

    check_items(
        &mut ast.items,
        &mut ast.type_env,
        &mut type_bindings,
        &mut value_bindings,
    )?;
    unimplemented!()
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
            Item::FnDef(FnDef { ref name, .. }) => {
                let ty = type_env.new_type_var();
                value_bindings.insert(name.value.clone(), Binding::new(ty));
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
    // the type binding must already have been created by `unify_types`
    let current_ty = *type_bindings.get(&def.name.value).unwrap();

    type_bindings.enter_scope();
    value_bindings.enter_scope();

    let mut param_names =
        FnvHashSet::with_capacity_and_hasher(def.params.len(), Default::default());
    let mut params = Vec::with_capacity(def.params.len());

    for param in &mut def.params {
        let param = &mut param.value;

        param.ty = match param.ty_hint.as_ref() {
            Some(hint) => type_from_desc(hint.as_ref(), false, type_env, type_bindings)?,
            None => type_env.new_type_var(),
        };

        // bind the parameter if it has a name
        if let Some(ref name) = &param.name.value {
            // do not allow the same param name twice
            if !param_names.insert(name) {
                return Err(Spanned::new(
                    TypeError::DuplicatParamName(name.clone()),
                    param.name.span,
                ));
            }

            value_bindings.insert(name.clone(), Binding::new(param.ty));
        }

        params.push(Param {
            name: param.name.value.clone(),
            ty: param.ty,
        });
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
        type_env,
        type_bindings,
        value_bindings,
    )
}

fn check_expr(
    expr: Spanned<&mut Expr>,
    ret_ty: TypeRef,
    type_env: &mut TypeEnv,
    type_bindings: &mut ScopeMap<Ident, TypeRef>,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<(), Spanned<TypeError>> {
    unimplemented!()
}

fn bind_record_def(
    def: &RecordDef,
    type_env: &mut TypeEnv,
    type_bindings: &ScopeMap<Ident, TypeRef>,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<(), Spanned<TypeError>> {
    // the type binding must already have been created by `unify_types`
    let current_ty = *type_bindings.get(&def.name.value).unwrap();

    let mut fields = Vec::with_capacity(def.fields.len());
    let mut cons_params = Vec::with_capacity(def.fields.len());

    for field_def in &def.fields {
        let field_def = &field_def.value;

        let name = field_def.name.value.clone();
        let ty = type_from_desc(field_def.ty.as_ref(), true, type_env, type_bindings)?;

        fields.push(Field {
            name: name.clone(),
            ty,
        });

        cons_params.push(Param {
            name: Some(name),
            ty,
        });
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
    value_bindings.insert(def.name.value.clone(), Binding::new(cons_ty));

    Ok(())
}

fn bind_variants_def(
    def: &VariantsDef,
    type_env: &mut TypeEnv,
    type_bindings: &ScopeMap<Ident, TypeRef>,
    value_bindings: &mut ScopeMap<Ident, Binding>,
) -> Result<(), Spanned<TypeError>> {
    // the type binding must already have been created by `unify_types`
    let current_ty = *type_bindings.get(&def.name.value).unwrap();

    let mut variants = Vec::with_capacity(def.variants.len());

    for variant_def in &def.variants {
        let variant_def = &variant_def.value;

        let params = variant_def
            .param_tys
            .iter()
            .map(|param_ty| type_from_desc(param_ty.as_ref(), true, type_env, type_bindings))
            .collect::<Result<Vec<_>, _>>()?;

        let cons_params = params
            .iter()
            .map(|&ty| Param { name: None, ty })
            .collect::<Vec<_>>();

        variants.push(Variant {
            name: variant_def.name.value.clone(),
            params,
        });

        let variant_cons = Function {
            params: cons_params,
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
                        .map(|ty| Param { name: None, ty })
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
