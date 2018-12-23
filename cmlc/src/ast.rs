use crate::span::Spanned;
use std::fmt::{self, Display};
use std::rc::Rc;

pub type Ast = Vec<Spanned<Item>>;

#[derive(Debug)]
pub enum Item {
    TypeDef(TypeDef),
    FnDef(FnDef),
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
    pub name: Spanned<Ident>,
    pub variants: Vec<Spanned<VariantDef>>,
}

#[derive(Debug)]
pub struct VariantDef {
    pub name: Spanned<Ident>,
    pub param_tys: Vec<Spanned<TypeDesc>>,
}

#[derive(Debug)]
pub struct FnDef {
    pub name: Spanned<Ident>,
    pub param_tys: Vec<Spanned<ParamDef>>,
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
    Hole,
    Name(Ident),
    Ptr(Box<TypeDesc>),
    Array(ArrayDesc),
    Function(FunctionDesc),
    Tuple(Vec<Spanned<TypeDesc>>),
}

#[derive(Debug)]
pub struct ArrayDesc {
    pub ty: Spanned<Box<TypeDesc>>,
    pub len: Spanned<u32>,
}

#[derive(Debug)]
pub struct FunctionDesc {
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
    Assignment(Assignment),
    LetBinding(LetBinding),
    Ret(Box<Expr>),
    IfExpr(IfExpr),
    Block(Block),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit {
    Str(Rc<str>),
    Int(u32),
    Bool(bool),
}

#[derive(Debug)]
pub struct BinOp {
    pub op: BinOpKind,
    pub lhs: Spanned<Box<Expr>>,
    pub rhs: Spanned<Box<Expr>>,
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
    Deref,
}

#[derive(Debug)]
pub struct FieldAccess {
    pub value: Spanned<Box<Expr>>,
    pub field: Spanned<Ident>,
    pub num_derefs: u32,
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
    pub name: Option<Spanned<Ident>>,
    pub value: Spanned<Expr>,
}

#[derive(Debug)]
pub struct ItemPath {
    pub segments: Vec<Spanned<Ident>>,
}

impl Display for ItemPath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string_repr = self
            .segments
            .iter()
            .map(|spanned| spanned.value.to_string())
            .collect::<Vec<_>>()
            .join("::");

        write!(f, "{}", string_repr)
    }
}

#[derive(Debug)]
pub struct MethodCall {
    pub object: Spanned<Box<Expr>>,
    pub call: Spanned<FnCall>,
}

#[derive(Debug)]
pub struct Assignment {
    pub var_name: Spanned<Ident>,
    pub value: Spanned<Box<Expr>>,
}

#[derive(Debug)]
pub struct LetBinding {
    pub pattern: Spanned<Pattern>,
    pub ty_hint: Option<Spanned<TypeDesc>>,
    pub expr: Spanned<Box<Expr>>,
}

#[derive(Debug)]
pub enum Pattern {
    Discard,
    Binding(Spanned<Ident>),
    MutBinding(Spanned<Ident>),
    Tuple(Spanned<Vec<Pattern>>),
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Spanned<Box<Expr>>,
    pub then_block: Spanned<Block>,
    pub else_block: Option<Spanned<Block>>,
}

#[derive(Debug)]
pub struct Block {
    pub exprs: Vec<Spanned<Expr>>,
    pub is_last_expr_stmt: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(Rc<str>);

impl Ident {
    pub fn new(ident: impl AsRef<str>) -> Ident {
        Ident(Rc::from(ident.as_ref()))
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
