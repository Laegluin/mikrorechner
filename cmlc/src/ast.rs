use crate::lexer;
use crate::span::Spanned;
use std::rc::Rc;

type Ast = Vec<Spanned<Item>>;

pub enum Item {
    TypeDef(Spanned<TypeDef>),
    FnDef(Spanned<FnDef>),
}

pub enum TypeDef {
    Alias(Spanned<TypeDesc>),
    RecordDef(Spanned<RecordDef>),
    VariantsDef(Spanned<VariantsDef>),
}

pub struct RecordDef {
    pub name: Spanned<Ident>,
    pub fields: Vec<Spanned<FieldDef>>,
}

pub struct FieldDef {
    pub name: Spanned<Ident>,
    pub ty: Spanned<TypeDesc>,
}

pub struct VariantsDef {
    pub name: Spanned<FieldDef>,
    pub variants: Vec<Spanned<VariantDef>>,
}

pub struct VariantDef {
    pub name: Spanned<Ident>,
    pub params_ty: Vec<Spanned<TypeDesc>>,
}

pub struct FnDef {
    pub name: Spanned<Ident>,
    pub params_ty: Vec<Spanned<ParamDef>>,
    pub ret_ty: Spanned<TypeDesc>,
    pub body: Spanned<Block>,
}

pub struct ParamDef {
    pub name: Spanned<Option<Ident>>,
    pub ty_name: Spanned<TypeDesc>,
}

pub enum TypeDesc {
    Name(Ident),
    Ptr(Box<TypeDesc>),
    Function(FunctionDesc),
    Tuple(Vec<Spanned<TypeDesc>>),
}

pub struct FunctionDesc {
    pub name: Spanned<Ident>,
    pub params_ty: Vec<Spanned<TypeDesc>>,
    pub ret_ty: Spanned<Box<TypeDesc>>,
}

pub enum Expr {
    Lit(lexer::Lit),
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

pub struct BinOp {
    op: BinOpKind,
    lhs: Spanned<Box<Expr>>,
    rhs: Spanned<Box<Expr>>,
}

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

pub struct UnOp {
    pub op: UnOpKind,
    pub operand: Spanned<Box<Expr>>,
}

pub enum UnOpKind {
    Not,
    AddrOf,
    AddrOfMut,
}

pub struct FieldAccess {
    pub value: Spanned<Ident>,
    pub field: Spanned<Ident>,
    pub chained_fields: Vec<Spanned<Ident>>,
}

pub struct ArrayCons {
    pub elems: Vec<Spanned<Expr>>,
}

pub struct TupleCons {
    pub elems: Vec<Spanned<Expr>>,
}

pub struct RecordCons {
    pub name: Spanned<Ident>,
    pub fields: Vec<Spanned<FieldCons>>,
}

pub struct FieldCons {
    pub name: Spanned<Ident>,
    pub value: Spanned<Expr>,
}

pub struct FnCall {
    pub name: Spanned<ItemPath>,
    pub args: Vec<Spanned<Arg>>,
}

pub struct Arg {
    pub name: Spanned<Option<Ident>>,
    pub value: Spanned<Expr>,
}

pub struct ItemPath {
    pub segments: Vec<Spanned<Ident>>,
}

pub struct MethodCall {
    pub object: Spanned<Box<Expr>>,
    pub call: Spanned<FnCall>,
}

pub struct LetBinding {
    pub pattern: Spanned<Pattern>,
    pub expr: Spanned<Box<Expr>>,
}

pub enum Pattern {
    Binding(Spanned<Ident>),
    MutBinding(Spanned<Ident>),
    TupleDestructuring(Spanned<Vec<Pattern>>),
}

pub struct Block {
    pub exprs: Vec<Spanned<Expr>>,
}

#[derive(Debug)]
pub struct Ident(Rc<str>);

impl Ident {
    pub fn new(ident: impl AsRef<str>) -> Ident {
        Ident(Rc::from(ident.as_ref()))
    }
}
