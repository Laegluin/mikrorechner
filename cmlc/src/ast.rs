use crate::span::{Span, Spanned};
use crate::typecheck::{unify::TypeEnv, Type, TypeDesc, TypeRef};
use fnv::FnvHashMap;
use std::fmt::{self, Display, Write};
use std::rc::Rc;

#[derive(Debug)]
pub struct TypedAst {
    pub items: Vec<Spanned<Item>>,
    pub types: FnvHashMap<TypeRef, Type>,
}

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
}

#[derive(Debug)]
pub struct AliasDef {
    pub name: Spanned<Ident>,
    pub ty: Spanned<TypeDecl>,
}

#[derive(Debug)]
pub struct RecordDef {
    pub name: Spanned<Ident>,
    pub fields: Vec<Spanned<FieldDef>>,
}

#[derive(Debug)]
pub struct FieldDef {
    pub name: Spanned<Ident>,
    pub ty: Spanned<TypeDecl>,
}

#[derive(Debug)]
pub struct FnDef {
    pub name: ItemPath,
    pub params: Vec<Spanned<ParamDef>>,
    pub ret_ty_hint: Option<Spanned<TypeDecl>>,
    pub ret_ty: TypeRef,
    pub body: Spanned<Expr>,
}

impl FnDef {
    pub fn desc(&self) -> TypeDesc {
        let params_desc = self
            .params
            .iter()
            .map(|param| param.value.ty.desc())
            .collect();

        TypeDesc::ConstPtr(Rc::new(TypeDesc::Function(params_desc, self.ret_ty.desc())))
    }

    pub fn signature(&self) -> String {
        let inner = || -> Result<_, fmt::Error> {
            if self.params.is_empty() {
                return Ok(format!("fn {} -> {}", self.name, self.ret_ty));
            }

            let mut buf = String::new();
            write!(buf, "fn {} ", self.name)?;

            let param_str = self
                .params
                .iter()
                .map(|param| {
                    let param = &param.value;
                    let name = param
                        .name
                        .value
                        .as_ref()
                        .map(Ident::to_string)
                        .unwrap_or_else(|| String::from("_"));

                    format!("{}: {}", name, param.ty)
                })
                .collect::<Vec<_>>()
                .join(", ");

            write!(buf, "{} -> {}", param_str, self.ret_ty)?;

            Ok(buf)
        };

        inner().unwrap()
    }
}

#[derive(Debug)]
pub struct ParamDef {
    pub name: Spanned<Option<Ident>>,
    pub ty_hint: Option<Spanned<TypeDecl>>,
    pub ty: TypeRef,
}

impl ParamDef {
    pub fn record_cons_param(field: &FieldDef, ty: TypeRef) -> ParamDef {
        ParamDef {
            name: field.name.clone().map(Some),
            ty_hint: Some(field.ty.clone()),
            ty,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeDecl {
    Hole,
    Name(Ident),
    ConstPtr(Spanned<Box<TypeDecl>>),
    MutPtr(Spanned<Box<TypeDecl>>),
    Array(ArrayDecl),
    Function(FunctionDecl),
    Tuple(Vec<Spanned<TypeDecl>>),
}

#[derive(Debug, Clone)]
pub struct ArrayDecl {
    pub ty: Spanned<Box<TypeDecl>>,
    pub len: Spanned<u32>,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub param_tys: Vec<Spanned<TypeDecl>>,
    pub ret_ty: Spanned<Box<TypeDecl>>,
}

#[derive(Debug)]
pub enum Expr {
    Lit(Lit, TypeRef),
    Var(ItemPath, TypeRef),
    UnOp(UnOp, TypeRef),
    Cast(Cast, TypeRef),
    BinOp(BinOp, TypeRef),
    FnCall(FnCall, TypeRef),
    MemberAccess(MemberAccess, TypeRef),
    ArrayCons(ArrayCons, TypeRef),
    TupleCons(TupleCons, TypeRef),
    Assignment(Assignment, TypeRef),
    LetBinding(LetBinding, TypeRef),
    AutoRef(Box<Expr>, TypeRef),
    Ret(Option<Spanned<Box<Expr>>>, TypeRef),
    IfExpr(IfExpr, TypeRef),
    WhileExpr(WhileExpr, TypeRef),
    Block(Block, TypeRef),
    /// Placeholder expression representing the body of a record constructor function.
    ConstructRecord(TypeRef),
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

    pub fn cast(cast: Cast) -> Expr {
        Expr::Cast(cast, TypeRef::invalid())
    }

    pub fn bin_op(bin_op: BinOp) -> Expr {
        Expr::BinOp(bin_op, TypeRef::invalid())
    }

    pub fn fn_call(fn_call: FnCall) -> Expr {
        Expr::FnCall(fn_call, TypeRef::invalid())
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

    pub fn auto_ref(auto_ref: Box<Expr>) -> Expr {
        Expr::AutoRef(auto_ref, TypeRef::invalid())
    }

    pub fn ret(ret: Option<Spanned<Box<Expr>>>) -> Expr {
        Expr::Ret(ret, TypeRef::invalid())
    }

    pub fn if_expr(if_expr: IfExpr) -> Expr {
        Expr::IfExpr(if_expr, TypeRef::invalid())
    }

    pub fn while_expr(while_expr: WhileExpr) -> Expr {
        Expr::WhileExpr(while_expr, TypeRef::invalid())
    }

    pub fn block(block: Block) -> Expr {
        Expr::Block(block, TypeRef::invalid())
    }

    pub fn construct_record() -> Expr {
        Expr::ConstructRecord(TypeRef::invalid())
    }

    pub fn ty(&self) -> &TypeRef {
        match *self {
            Expr::Lit(_, ref ty) => ty,
            Expr::Var(_, ref ty) => ty,
            Expr::UnOp(_, ref ty) => ty,
            Expr::Cast(_, ref ty) => ty,
            Expr::BinOp(_, ref ty) => ty,
            Expr::FnCall(_, ref ty) => ty,
            Expr::MemberAccess(_, ref ty) => ty,
            Expr::ArrayCons(_, ref ty) => ty,
            Expr::TupleCons(_, ref ty) => ty,
            Expr::Assignment(_, ref ty) => ty,
            Expr::LetBinding(_, ref ty) => ty,
            Expr::AutoRef(_, ref ty) => ty,
            Expr::Ret(_, ref ty) => ty,
            Expr::IfExpr(_, ref ty) => ty,
            Expr::WhileExpr(_, ref ty) => ty,
            Expr::Block(_, ref ty) => ty,
            Expr::ConstructRecord(ref ty) => ty,
        }
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub enum UnOpKind {
    Not,
    Negate,
    AddrOf,
    AddrOfMut,
    Deref,
}

#[derive(Debug)]
pub struct Cast {
    pub expr: Spanned<Box<Expr>>,
    pub to_ty: Spanned<TypeDecl>,
}

#[derive(Debug)]
pub struct MemberAccess {
    pub value: Spanned<Box<Expr>>,
    pub member: Spanned<Ident>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemPath {
    segments: Vec<Spanned<Ident>>,
}

impl ItemPath {
    pub fn name(&self) -> &Ident {
        self.segments[self.segments.len() - 1].as_ref().value
    }

    pub fn span(&self) -> Span {
        self.segments[0]
            .span
            .to(self.segments[self.segments.len() - 1].span)
    }
}

impl From<Vec<Spanned<Ident>>> for ItemPath {
    fn from(idents: Vec<Spanned<Ident>>) -> ItemPath {
        assert!(!idents.is_empty());
        ItemPath { segments: idents }
    }
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
pub struct Assignment {
    pub target: Spanned<Box<Expr>>,
    pub value: Spanned<Box<Expr>>,
}

#[derive(Debug)]
pub struct LetBinding {
    pub pattern: Spanned<Pattern>,
    pub ty_hint: Option<Spanned<TypeDecl>>,
    pub expr: Spanned<Box<Expr>>,
}

#[derive(Debug)]
pub enum Pattern {
    Discard(TypeRef),
    Binding(Spanned<Ident>, TypeRef),
    MutBinding(Spanned<Ident>, TypeRef),
    Tuple(Vec<Spanned<Pattern>>, TypeRef),
}

impl Pattern {
    pub fn discard() -> Pattern {
        Pattern::Discard(TypeRef::invalid())
    }

    pub fn binding(name: Spanned<Ident>) -> Pattern {
        Pattern::Binding(name, TypeRef::invalid())
    }

    pub fn mut_binding(name: Spanned<Ident>) -> Pattern {
        Pattern::MutBinding(name, TypeRef::invalid())
    }

    pub fn tuple(patterns: Vec<Spanned<Pattern>>) -> Pattern {
        Pattern::Tuple(patterns, TypeRef::invalid())
    }
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Spanned<Box<Expr>>,
    pub then_block: Spanned<Box<Expr>>,
    pub else_block: Option<Spanned<Box<Expr>>>,
}

#[derive(Debug)]
pub struct WhileExpr {
    pub cond: Spanned<Box<Expr>>,
    pub body: Spanned<Box<Expr>>,
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

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl PartialEq<&str> for Ident {
    fn eq(&self, rhs: &&str) -> bool {
        &*self.0 == *rhs
    }
}
