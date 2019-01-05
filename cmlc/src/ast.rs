use crate::span::Spanned;
use crate::typecheck::{unify::TypeEnv, TypeRef};
use std::fmt::{self, Display};
use std::rc::Rc;

#[derive(Debug)]
pub struct Ast {
    pub items: Vec<Spanned<Item>>,
    pub type_env: TypeEnv,
}

impl Ast {
    pub fn new(items: Vec<Spanned<Item>>) -> Ast {
        Ast {
            items,
            type_env: TypeEnv::new(),
        }
    }
}

#[derive(Debug)]
pub enum Item {
    TypeDef(TypeDef),
    FnDef(FnDef),
}

#[derive(Debug)]
pub enum TypeDef {
    Alias(Spanned<AliasDef>),
    RecordDef(Spanned<RecordDef>),
    VariantsDef(Spanned<VariantsDef>),
}

#[derive(Debug)]
pub struct AliasDef {
    pub name: Spanned<Ident>,
    pub ty: Spanned<TypeDesc>,
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
    pub params: Vec<Spanned<ParamDef>>,
    pub ret_ty_hint: Option<Spanned<TypeDesc>>,
    pub ret_ty: TypeRef,
    pub body: Spanned<Expr>,
}

#[derive(Debug)]
pub struct ParamDef {
    pub name: Spanned<Option<Ident>>,
    pub ty_hint: Option<Spanned<TypeDesc>>,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub enum TypeDesc {
    Hole,
    Name(Ident),
    ConstPtr(Spanned<Box<TypeDesc>>),
    MutPtr(Spanned<Box<TypeDesc>>),
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
    pub param_tys: Vec<Spanned<TypeDesc>>,
    pub ret_ty: Spanned<Box<TypeDesc>>,
}

#[derive(Debug)]
pub enum Expr {
    Lit(Lit, TypeRef),
    Var(ItemPath, TypeRef),
    UnOp(UnOp, TypeRef),
    BinOp(BinOp, TypeRef),
    FnCall(FnCall, TypeRef),
    MethodCall(MethodCall, TypeRef),
    MemberAccess(MemberAccess, TypeRef),
    ArrayCons(ArrayCons, TypeRef),
    TupleCons(TupleCons, TypeRef),
    Assignment(Assignment, TypeRef),
    LetBinding(LetBinding, TypeRef),
    Ret(Box<Expr>, TypeRef),
    IfExpr(IfExpr, TypeRef),
    Block(Block, TypeRef),
}

impl Expr {
    pub fn lit(lit: Lit) -> Expr {
        Expr::Lit(lit, TypeRef::invalid())
    }

    pub fn var(var: ItemPath) -> Expr {
        Expr::Var(var, TypeRef::invalid())
    }

    pub fn un_op(un_op: UnOp) -> Expr {
        Expr::UnOp(un_op, TypeRef::invalid())
    }

    pub fn bin_op(bin_op: BinOp) -> Expr {
        Expr::BinOp(bin_op, TypeRef::invalid())
    }

    pub fn fn_call(fn_call: FnCall) -> Expr {
        Expr::FnCall(fn_call, TypeRef::invalid())
    }

    pub fn method_call(method_call: MethodCall) -> Expr {
        Expr::MethodCall(method_call, TypeRef::invalid())
    }

    pub fn member_access(member_access: MemberAccess) -> Expr {
        Expr::MemberAccess(member_access, TypeRef::invalid())
    }

    pub fn array_cons(array_cons: ArrayCons) -> Expr {
        Expr::ArrayCons(array_cons, TypeRef::invalid())
    }

    pub fn tuple_cons(tuple_cons: TupleCons) -> Expr {
        Expr::TupleCons(tuple_cons, TypeRef::invalid())
    }

    pub fn assignment(assignment: Assignment) -> Expr {
        Expr::Assignment(assignment, TypeRef::invalid())
    }

    pub fn let_binding(let_binding: LetBinding) -> Expr {
        Expr::LetBinding(let_binding, TypeRef::invalid())
    }

    pub fn ret(ret: Box<Expr>) -> Expr {
        Expr::Ret(ret, TypeRef::invalid())
    }

    pub fn if_expr(if_expr: IfExpr) -> Expr {
        Expr::IfExpr(if_expr, TypeRef::invalid())
    }

    pub fn block(block: Block) -> Expr {
        Expr::Block(block, TypeRef::invalid())
    }
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
    Or,
    And,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub struct UnOp {
    pub op: UnOpKind,
    pub operand: Spanned<Box<Expr>>,
}

#[derive(Debug)]
pub enum UnOpKind {
    Not,
    Negate,
    AddrOf,
    AddrOfMut,
    Deref,
}

#[derive(Debug)]
pub struct MemberAccess {
    pub value: Spanned<Box<Expr>>,
    pub member: Spanned<Ident>,
    pub is_deref: bool,
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
pub struct FnCall {
    pub name: Spanned<ItemPath>,
    pub args: Vec<Spanned<Arg>>,
}

#[derive(Debug)]
pub struct Arg {
    pub name: Option<Spanned<Ident>>,
    pub value: Spanned<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ItemPath {
    pub segments: Vec<Spanned<Ident>>,
}

impl From<Spanned<Ident>> for ItemPath {
    fn from(ident: Spanned<Ident>) -> ItemPath {
        ItemPath {
            segments: vec![ident],
        }
    }
}

impl IntoIterator for ItemPath {
    type Item = Ident;
    type IntoIter = std::iter::Map<std::vec::IntoIter<Spanned<Ident>>, fn(Spanned<Ident>) -> Ident>;

    fn into_iter(self) -> Self::IntoIter {
        self.segments.into_iter().map(Spanned::into_inner)
    }
}

impl<'a> IntoIterator for &'a ItemPath {
    type Item = &'a Ident;
    type IntoIter =
        std::iter::Map<std::slice::Iter<'a, Spanned<Ident>>, fn(&'a Spanned<Ident>) -> &'a Ident>;

    fn into_iter(self) -> Self::IntoIter {
        self.segments.iter().map(|spanned| &spanned.value)
    }
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
    pub target: Spanned<Box<Expr>>,
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
    pub then_block: Spanned<Box<Expr>>,
    pub else_block: Option<Spanned<Box<Expr>>>,
}

#[derive(Debug)]
pub struct Block {
    pub exprs: Vec<Spanned<Expr>>,
    pub is_last_expr_stmt: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
