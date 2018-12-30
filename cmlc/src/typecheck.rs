use crate::ast::{Ast, Ident, ItemPath};
use std::mem;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, PartialEq)]
pub enum Type {
    Never,
    Bool,
    I32,
    U32,
    Str,
    Ptr(Box<Type>),
    MutPtr(Box<Type>),
    Array(Array),
    Tuple(Vec<Type>),
    Function(Function),
    Record(Record),
    Variants(Variants),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

#[derive(Debug, PartialEq)]
pub struct Array {
    pub ty: Box<Type>,
    pub len: u32,
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
    pub ty: Type,
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
    pub params: Vec<Type>,
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
pub struct TypeVarRef(usize);

#[derive(Debug)]
pub enum TypeVar {
    /// A free type variable.
    Any,
    /// An unspecified integer type.
    Int,
    Never,
    Bool,
    I32,
    U32,
    Str,
    Ptr(TypeVarRef),
    MutPtr(TypeVarRef),
    Array(TypeVarRef, u32),
    Tuple(Vec<TypeVarRef>),
    Function(Function),
    Record(Record),
    Variants(Variants),
}

#[derive(Debug)]
pub enum Typed {
    None,
    Var(TypeVarRef),
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
/// Notably, there is subtyping with Never and primitives (Int can be subtyped to more specific integers,
/// otherwise a default is chosen).
struct CheckEnv {
    nodes: Vec<Node>,
}

enum Node {
    Root(Root),
    Next(usize),
}

struct Root {
    var: TypeVar,
    rank: u8,
}

impl CheckEnv {
    fn new() -> CheckEnv {
        CheckEnv { nodes: Vec::new() }
    }

    fn insert_type(&mut self, var: TypeVar) -> TypeVarRef {
        self.nodes.push(Node::Root(Root { var, rank: 0 }));
        TypeVarRef(self.nodes.len() - 1)
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

    fn union(&mut self, left: TypeVarRef, right: TypeVarRef) -> Result<TypeVarRef, TypeError> {
        let (left_idx, left_root) = self.find(left.0);
        let left_rank = left_root.rank;
        let left_var = mem::replace(&mut left_root.var, TypeVar::Any);

        let (right_idx, right_root) = self.find(right.0);
        let right_rank = right_root.rank;
        let right_var = mem::replace(&mut right_root.var, TypeVar::Any);

        if left_idx == right_idx {
            return Ok(TypeVarRef(left_idx));
        }

        let merged = self.merge(left_var, right_var)?;

        if left_rank < right_rank {
            self.nodes[left_idx] = Node::Next(right_idx);
            self.nodes[right_idx] = Node::Root(Root {
                var: merged,
                rank: right_rank,
            });

            Ok(TypeVarRef(right_idx))
        } else {
            self.nodes[right_idx] = Node::Next(left_idx);
            self.nodes[left_idx] = Node::Root(Root {
                var: merged,
                rank: if left_rank == right_rank {
                    left_rank + 1
                } else {
                    left_rank
                },
            });

            Ok(TypeVarRef(left_idx))
        }
    }

    fn merge(&mut self, left: TypeVar, right: TypeVar) -> Result<TypeVar, TypeError> {
        match (left, right) {
            // always choose the other one, it can never be less specific
            (TypeVar::Any, other) | (other, TypeVar::Any) => Ok(other),
            // Never is the supertype of all other types (except Any), so use the more specific subtype
            (TypeVar::Never, other) | (other, TypeVar::Never) => Ok(other),
            // choose the more specific int
            (TypeVar::Int, int @ TypeVar::U32)
            | (TypeVar::Int, int @ TypeVar::I32)
            | (int @ TypeVar::U32, TypeVar::Int)
            | (int @ TypeVar::I32, TypeVar::Int) => Ok(int),
            // all non-generic types can always be merged as themselves
            (eq @ TypeVar::Int, TypeVar::Int)
            | (eq @ TypeVar::Bool, TypeVar::Bool)
            | (eq @ TypeVar::I32, TypeVar::I32)
            | (eq @ TypeVar::U32, TypeVar::U32)
            | (eq @ TypeVar::Str, TypeVar::Str) => Ok(eq),
            // for generic types, unify the type parameters first
            (TypeVar::Ptr(left_inner), TypeVar::Ptr(right_inner)) => {
                let inner = self.union(left_inner, right_inner)?;
                Ok(TypeVar::Ptr(inner))
            }
            (TypeVar::MutPtr(left_inner), TypeVar::MutPtr(right_inner)) => {
                let inner = self.union(left_inner, right_inner)?;
                Ok(TypeVar::MutPtr(inner))
            }
            (TypeVar::Array(left_inner, left_len), TypeVar::Array(right_inner, right_len)) => {
                if left_len != right_len {
                    // TODO: error
                    panic!();
                }

                let inner = self.union(left_inner, right_inner)?;
                Ok(TypeVar::Array(inner, left_len))
            }
            (TypeVar::Tuple(left_inner), TypeVar::Tuple(right_inner)) => {
                if left_inner.len() != right_inner.len() {
                    // TODO: error
                    panic!();
                }

                let inner = left_inner
                    .into_iter()
                    .zip(right_inner)
                    .map(|(left, right)| self.union(left, right))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(TypeVar::Tuple(inner))
            }
            // functions are required to have full type annotations, so the can simply be compared
            (TypeVar::Function(left_fn), TypeVar::Function(right_fn)) => {
                if left_fn == right_fn {
                    Ok(TypeVar::Function(left_fn))
                } else {
                    // TODO: error
                    panic!()
                }
            }
            // nominal types are always fully known, so the can also be compared directly
            (TypeVar::Record(left_record), TypeVar::Record(right_record)) => {
                if left_record == right_record {
                    Ok(TypeVar::Record(left_record))
                } else {
                    // TODO: error
                    panic!()
                }
            }
            (TypeVar::Variants(left_variants), TypeVar::Variants(right_variants)) => {
                if left_variants == right_variants {
                    Ok(TypeVar::Variants(left_variants))
                } else {
                    // TODO: error
                    panic!()
                }
            }
            _ => {
                // TODO: error
                panic!()
            }
        }
    }
}

#[derive(Debug)]
pub enum TypeError {}

pub fn typecheck(ast: Ast) -> Result<Ast, TypeError> {
    let mut check_env = CheckEnv::new();
    let mut type_env = ScopeMap::new();
    let mut value_env = ScopeMap::new();

    let ast = unify_types(ast, &mut check_env, &mut type_env, &mut value_env)?;
    unimplemented!()
}

fn unify_types(
    ast: Ast,
    check_env: &mut CheckEnv,
    type_env: &mut ScopeMap<ItemPath, Type>,
    value_env: &mut ScopeMap<ItemPath, TypeVarRef>,
) -> Result<Ast, TypeError> {
    unimplemented!()
}
