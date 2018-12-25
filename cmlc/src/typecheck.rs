use crate::ast::Ident;

pub enum Type {
    None,
    Never,
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

pub struct Function {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

pub struct Array {
    pub ty: Box<Type>,
    pub len: u32,
}

pub struct Record {
    pub name: Ident,
    pub fields: Vec<Field>,
}

pub struct Field {
    pub name: Ident,
    pub ty: Type,
}

pub struct Variants {
    pub name: Ident,
    pub variants: Vec<Variant>,
}

pub struct Variant {
    pub name: Ident,
    pub params: Vec<Type>,
}
