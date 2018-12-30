use crate::ast::Ident;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug)]
pub enum Type {
    Never,
    Unit,
    Bool,
    I32,
    U32,
    Str,
    Ptr(Box<Type>),
    MutPtr(Box<Type>),
    Function(Function),
    Array(Array),
    Tuple(Vec<Type>),
    Record(Record),
    Variants(Variants),
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

#[derive(Debug)]
pub struct Array {
    pub ty: Box<Type>,
    pub len: u32,
}

#[derive(Debug)]
pub struct Record {
    pub id: TypeId,
    pub name: Ident,
    pub fields: Vec<Field>,
}

#[derive(Debug)]
pub struct Field {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Variants {
    pub id: TypeId,
    pub name: Ident,
    pub variants: Vec<Variant>,
}

#[derive(Debug)]
pub struct Variant {
    pub name: Ident,
    pub params: Vec<Type>,
}

#[derive(Debug)]
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
    Unit,
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

pub struct ScopeMap<K, V> {
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
