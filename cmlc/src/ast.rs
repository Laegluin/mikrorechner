use crate::span::Spanned;
use std::rc::Rc;

pub type Ast = Vec<Spanned<Item>>;

#[derive(Debug)]
pub enum Item {
    TypeDef(Spanned<TypeDef>),
    FnDef(Spanned<FnDef>),
}

#[derive(Debug)]
pub enum TypeDef {
    Alias(Spanned<TypeDesc>),
    RecordDef(Spanned<RecordDef>),
    VariantsDef(Spanned<VariantsDef>),
}

#[derive(Debug)]
pub struct RecordDef {
    pub name: Spanned<Ident>,
    pub fields: Vec<Spanned<FieldDef>>,
}

#[derive(Debug)]
pub struct FieldDef {
    pub name: Spanned<Ident>,
    pub ty: Spanned<TypeDesc>,
}

#[derive(Debug)]
pub struct VariantsDef {
    pub name: Spanned<FieldDef>,
    pub variants: Vec<Spanned<VariantDef>>,
}

#[derive(Debug)]
pub struct VariantDef {
    pub name: Spanned<Ident>,
    pub params_ty: Vec<Spanned<TypeDesc>>,
}

#[derive(Debug)]
pub struct FnDef {
    pub name: Spanned<Ident>,
    pub params_ty: Vec<Spanned<ParamDef>>,
    pub ret_ty: Spanned<TypeDesc>,
    pub body: Spanned<Block>,
}

#[derive(Debug)]
pub struct ParamDef {
    pub name: Spanned<Option<Ident>>,
    pub ty_name: Spanned<TypeDesc>,
}

#[derive(Debug)]
pub enum TypeDesc {
    Name(Ident),
    Ptr(Box<TypeDesc>),
    Function(FunctionDesc),
    Tuple(Vec<Spanned<TypeDesc>>),
}

#[derive(Debug)]
pub struct FunctionDesc {
    pub name: Spanned<Ident>,
    pub params_ty: Vec<Spanned<TypeDesc>>,
    pub ret_ty: Spanned<Box<TypeDesc>>,
}

#[derive(Debug)]
pub enum Expr {
    Lit(Lit),
    Var(Ident),
    UnOp(UnOp),
    BinOp(BinOp),
    FnCall(FnCall),
    MethodCall(MethodCall),
    FieldAccess(FieldAccess),
    ArrayCons(ArrayCons),
    TupleCons(TupleCons),
    RecordCons(RecordCons),
    Stmt(Box<Expr>),
    LetBinding(LetBinding),
    Block(Block),
}

#[derive(Debug, Clone)]
pub enum Lit {
    Str(Rc<str>),
    Int(u32),
    Bool(bool),
}

#[derive(Debug)]
pub struct BinOp {
    op: BinOpKind,
    lhs: Spanned<Box<Expr>>,
    rhs: Spanned<Box<Expr>>,
}

#[derive(Debug)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Eq,
    Or,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug)]
pub struct UnOp {
    pub op: UnOpKind,
    pub operand: Spanned<Box<Expr>>,
}

#[derive(Debug)]
pub enum UnOpKind {
    Not,
    AddrOf,
    AddrOfMut,
}

#[derive(Debug)]
pub struct FieldAccess {
    pub value: Spanned<Ident>,
    pub field: Spanned<Ident>,
    pub chained_fields: Vec<Spanned<Ident>>,
}

#[derive(Debug)]
pub struct ArrayCons {
    pub elems: Vec<Spanned<Expr>>,
}

#[derive(Debug)]
pub struct TupleCons {
    pub elems: Vec<Spanned<Expr>>,
}

#[derive(Debug)]
pub struct RecordCons {
    pub name: Spanned<Ident>,
    pub fields: Vec<Spanned<FieldCons>>,
}

#[derive(Debug)]
pub struct FieldCons {
    pub name: Spanned<Ident>,
    pub value: Spanned<Expr>,
}

#[derive(Debug)]
pub struct FnCall {
    pub name: Spanned<ItemPath>,
    pub args: Vec<Spanned<Arg>>,
}

#[derive(Debug)]
pub struct Arg {
    pub name: Spanned<Option<Ident>>,
    pub value: Spanned<Expr>,
}

#[derive(Debug)]
pub struct ItemPath {
    pub segments: Vec<Spanned<Ident>>,
}

#[derive(Debug)]
pub struct MethodCall {
    pub object: Spanned<Box<Expr>>,
    pub call: Spanned<FnCall>,
}

#[derive(Debug)]
pub struct LetBinding {
    pub pattern: Spanned<Pattern>,
    pub expr: Spanned<Box<Expr>>,
}

#[derive(Debug)]
pub enum Pattern {
    Binding(Spanned<Ident>),
    MutBinding(Spanned<Ident>),
    TupleDestructuring(Spanned<Vec<Pattern>>),
}

#[derive(Debug)]
pub struct Block {
    pub exprs: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Ident(Rc<str>);

impl Ident {
    pub fn new(ident: impl AsRef<str>) -> Ident {
        Ident(Rc::from(ident.as_ref()))
    }
}
