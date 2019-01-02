use crate::ast::*;
use crate::span::Spanned;
use crate::support;
use std::collections::HashMap;
use std::mem;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Clone, Copy)]
pub struct TypeRef(usize);

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

#[derive(Debug)]
pub enum Typed {
    None,
    Var(TypeRef),
    Type(Type),
}

struct ScopeMap<K, V> {
    scopes: Vec<Vec<(K, V)>>,
}

impl<K, V> ScopeMap<K, V>
where
    K: Eq,
{
    pub fn new() -> ScopeMap<K, V> {
        ScopeMap {
            scopes: vec![Vec::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        } else {
            panic!("cannot exit global scope");
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        let current_scope = self.scopes.len() - 1;
        self.scopes[current_scope].push((key, value));
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev())
            .find(|&(ref k, _)| k == key)
            .map(|&(_, ref value)| value)
    }
}

/// The environment holding all types during type checking. The types are stored as disjoint-set
/// (see <https://en.wikipedia.org/wiki/Disjoint-set_data_structure>) backed by a Vec. Because
/// of this unused nodes are only freed when the whole set is dropped. This is fine, since there
/// won't be enough nodes for this to matter; in return, everything is stored in a nice, flat piece of memory.
///
/// Merging of two types is done using a modified type inference algorithm for a Hindley-Milner type system.
/// (see <https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Degrees_of_freedom_instantiating_the_rules>).
///
/// Notably, there is subtyping with Never and pointers and primitives (Int can be subtyped to more specific integers,
/// otherwise a default is chosen).
#[derive(Debug)]
pub struct TypeEnv {
    nodes: Vec<Node>,
}

#[derive(Debug)]
enum Node {
    Root(Root),
    Next(usize),
}

#[derive(Debug)]
struct Root {
    var: Type,
    rank: u8,
}

impl TypeEnv {
    pub fn new() -> TypeEnv {
        TypeEnv { nodes: Vec::new() }
    }

    fn insert(&mut self, var: Type) -> TypeRef {
        self.nodes.push(Node::Root(Root { var, rank: 0 }));
        TypeRef(self.nodes.len() - 1)
    }

    fn find(&mut self, node: usize) -> (usize, &mut Root) {
        let start_node = node;
        let mut node = node;

        // using the ugly iterative version is required to make this compile while also using path compression,
        // because compressing the path requires a non overlapping write to the vec
        loop {
            match self.nodes[node] {
                Node::Next(next_node) => node = next_node,
                Node::Root(_) => {
                    // use path compression
                    if start_node != node {
                        self.nodes[start_node] = Node::Next(node);
                    }

                    match self.nodes[node] {
                        Node::Root(ref mut root) => return (node, root),
                        // we already confirmed the variant above, and neither
                        // `node` nor the actual value in the vec have changed
                        _ => unreachable!(),
                    }
                }
            }
        }
    }

    fn union(&mut self, expected: TypeRef, actual: TypeRef) -> Result<TypeRef, TypeError> {
        let (expected_idx, expected_root) = self.find(expected.0);
        let expected_rank = expected_root.rank;
        let expected_var = mem::replace(&mut expected_root.var, Type::Var);

        let (actual_idx, actual_root) = self.find(actual.0);
        let actual_rank = actual_root.rank;
        let actual_var = mem::replace(&mut actual_root.var, Type::Var);

        if expected_idx == actual_idx {
            return Ok(TypeRef(expected_idx));
        }

        let merged = self.merge(expected_var, actual_var)?;

        if expected_rank < actual_rank {
            self.nodes[expected_idx] = Node::Next(actual_idx);
            self.nodes[actual_idx] = Node::Root(Root {
                var: merged,
                rank: actual_rank,
            });

            Ok(TypeRef(actual_idx))
        } else {
            self.nodes[actual_idx] = Node::Next(expected_idx);
            self.nodes[expected_idx] = Node::Root(Root {
                var: merged,
                rank: if expected_rank == actual_rank {
                    expected_rank + 1
                } else {
                    expected_rank
                },
            });

            Ok(TypeRef(expected_idx))
        }
    }

    // TODO: impl error handling
    fn merge(&mut self, expected: Type, actual: Type) -> Result<Type, TypeError> {
        match (expected, actual) {
            // always choose the other one, it can never be less specific
            (Type::Var, other) | (other, Type::Var) => Ok(other),
            // Never is the supertype of all other types (except Any), so use the more specific subtype
            (Type::Never, other) | (other, Type::Never) => Ok(other),
            // choose the more specific int
            (Type::Int, int @ Type::U32)
            | (Type::Int, int @ Type::I32)
            | (int @ Type::U32, Type::Int)
            | (int @ Type::I32, Type::Int) => Ok(int),
            // all non-generic types can always be merged as themselves
            (eq @ Type::Int, Type::Int)
            | (eq @ Type::Bool, Type::Bool)
            | (eq @ Type::I32, Type::I32)
            | (eq @ Type::U32, Type::U32)
            | (eq @ Type::Str, Type::Str) => Ok(eq),
            // pointer coerces to the more specific pointer type (const, mut or it stays a generic pointer)
            (Type::Ptr(expected_inner), Type::Ptr(actual_inner)) => {
                let inner = self.union(expected_inner, actual_inner)?;
                Ok(Type::Ptr(inner))
            }
            (Type::Ptr(expected_inner), Type::MutPtr(actual_inner))
            | (Type::MutPtr(expected_inner), Type::Ptr(actual_inner)) => {
                let inner = self.union(expected_inner, actual_inner)?;
                Ok(Type::MutPtr(inner))
            }
            (Type::Ptr(expected_inner), Type::ConstPtr(actual_inner))
            | (Type::ConstPtr(expected_inner), Type::Ptr(actual_inner)) => {
                let inner = self.union(expected_inner, actual_inner)?;
                Ok(Type::ConstPtr(inner))
            }
            // mut pointers can be coerced to const pointers
            (Type::ConstPtr(expected_inner), Type::ConstPtr(actual_inner))
            | (Type::ConstPtr(expected_inner), Type::MutPtr(actual_inner))
            | (Type::MutPtr(expected_inner), Type::ConstPtr(actual_inner)) => {
                let inner = self.union(expected_inner, actual_inner)?;
                Ok(Type::ConstPtr(inner))
            }
            // mut pointers are only equal to themselves (given equal pointees)
            (Type::MutPtr(expected_inner), Type::MutPtr(actual_inner)) => {
                let inner = self.union(expected_inner, actual_inner)?;
                Ok(Type::MutPtr(inner))
            }
            // for generic types, unify the type parameters first
            (Type::Array(expected_inner, expected_len), Type::Array(actual_inner, actual_len)) => {
                if expected_len != actual_len {
                    unimplemented!();
                }

                let inner = self.union(expected_inner, actual_inner)?;
                Ok(Type::Array(inner, expected_len))
            }
            (Type::Tuple(expected_inner), Type::Tuple(actual_inner)) => {
                if expected_inner.len() != actual_inner.len() {
                    unimplemented!();
                }

                let inner = expected_inner
                    .into_iter()
                    .zip(actual_inner)
                    .map(|(expected, actual)| self.union(expected, actual))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Type::Tuple(inner))
            }
            (Type::Function(func), Type::Call(call)) | (Type::Call(call), Type::Function(func)) => {
                if func.params.len() != call.params.len() {
                    unimplemented!();
                }

                let mut params: Vec<_> = func.params.into_iter().enumerate().collect();
                let mut unified_params = Vec::with_capacity(params.len());

                // collect the named args first, since they determine the order of the unnamed args
                for arg in call.params.iter().filter(|param| param.name.is_some()) {
                    let (idx, mut param) =
                        support::find_remove(&mut params, |(_, param)| param.name == arg.name)
                            .unwrap_or_else(|| unimplemented!());

                    param.ty = self.union(param.ty, arg.ty)?;
                    unified_params.push((idx, param));
                }

                // unify the remaining unnamed args from left to right
                for (arg, (idx, mut param)) in call
                    .params
                    .iter()
                    .filter(|param| param.name.is_none())
                    .zip(params)
                {
                    param.ty = self.union(param.ty, arg.ty)?;
                    unified_params.push((idx, param));
                }

                // sort the params in the order of the definition, otherwise the type
                // of the function could have changed due to named arguments
                unified_params.sort_unstable_by_key(|&(idx, _)| idx);
                let unified_params: Vec<_> =
                    unified_params.into_iter().map(|(_, param)| param).collect();

                let ret = self.union(func.ret, call.ret)?;

                Ok(Type::Function(Function {
                    params: unified_params,
                    ret,
                }))
            }
            // unify all params and the return type. For params, the names have to be
            // equal or not exist at all. If only one param name exists, it is coerced
            // to an unnamed parameter.
            (Type::Function(expected_fn), Type::Function(actual_fn)) => {
                if expected_fn.params.len() != actual_fn.params.len() {
                    unimplemented!();
                }

                let params = expected_fn
                    .params
                    .into_iter()
                    .zip(actual_fn.params)
                    .map(|(expected, actual)| {
                        let name = match (expected.name, actual.name) {
                            (Some(expected), Some(actual)) => {
                                if expected != actual {
                                    unimplemented!();
                                }

                                Some(expected)
                            }
                            (Some(_), None) | (None, Some(_)) => None,
                            (None, None) => None,
                        };

                        let ty = self.union(expected.ty, actual.ty)?;
                        Ok(Param { name, ty })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let ret = self.union(expected_fn.ret, actual_fn.ret)?;

                Ok(Type::Function(Function { params, ret }))
            }
            // nominal types are always fully known, so they can be compared directly
            (Type::Record(expected_record), Type::Record(actual_record)) => {
                if expected_record == actual_record {
                    Ok(Type::Record(expected_record))
                } else {
                    unimplemented!()
                }
            }
            (Type::Variants(expected_variants), Type::Variants(actual_variants)) => {
                if expected_variants == actual_variants {
                    Ok(Type::Variants(expected_variants))
                } else {
                    unimplemented!()
                }
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub enum TypeError {
    HoleInTypeDef,
    UndefinedType(ItemPath),
}

pub fn typecheck(ast: Ast) -> Result<Ast, TypeError> {
    let mut type_bindings = ScopeMap::new();
    let mut value_bindings = ScopeMap::new();

    let ast = unify_types(ast, &mut type_bindings, &mut value_bindings)?;
    unimplemented!()
}

#[derive(Debug)]
enum UnresolvedTypeDef<'a> {
    Alias(&'a Spanned<TypeDesc>),
    RecordDef(&'a [Spanned<FieldDef>]),
    VariantsDef(&'a [Spanned<VariantDef>]),
}

fn unify_types(
    ast: Ast,
    type_bindings: &mut ScopeMap<ItemPath, Type>,
    value_bindings: &mut ScopeMap<ItemPath, TypeRef>,
) -> Result<Ast, TypeError> {
    let mut type_defs = HashMap::new();

    // collect all type defs, but delay resolution to allow for (mutally) recursive definitions
    // TODO: detect shadowed types
    for item in &ast.items {
        match item.value {
            Item::TypeDef(TypeDef::Alias(Spanned {
                value: AliasDef { ref name, ref ty },
                ..
            })) => {
                type_defs.insert(name, UnresolvedTypeDef::Alias(ty));
            }
            Item::TypeDef(TypeDef::RecordDef(Spanned {
                value:
                    RecordDef {
                        ref name,
                        ref fields,
                    },
                ..
            })) => {
                type_defs.insert(name, UnresolvedTypeDef::RecordDef(fields));
            }
            Item::TypeDef(TypeDef::VariantsDef(Spanned {
                value:
                    VariantsDef {
                        ref name,
                        ref variants,
                    },
                ..
            })) => {
                type_defs.insert(name, UnresolvedTypeDef::VariantsDef(variants));
            }
            // no point in resolving functions before all types are known
            Item::FnDef(_) => continue,
        }
    }

    // generate the types for all type definitions
    for (ty_name, ty_def) in &type_defs {
        match ty_def {
            UnresolvedTypeDef::Alias(ref alias) => unimplemented!(),
            _ => unimplemented!(),
        }
    }

    unimplemented!()
}

// TODO: do not allow duplicate parameter names

fn type_from_desc(
    desc: Spanned<&TypeDesc>,
    is_type_def: bool,
    type_env: &mut TypeEnv,
    type_bindings: &ScopeMap<ItemPath, TypeRef>,
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
        TypeDesc::Name(ref ident) => {
            let path = ItemPath::from(Spanned::new(ident.clone(), span));

            *type_bindings
                .get(&path)
                .ok_or_else(|| Spanned::new(TypeError::UndefinedType(path), span))?
        }
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

            type_env.insert(Type::Function(Function { params, ret }))
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
