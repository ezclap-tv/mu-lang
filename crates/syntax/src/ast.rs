use indexmap::IndexMap;

use crate::lexer::Token;

pub type Map<'a, T> = IndexMap<Ident<'a>, T>;

pub type Ident<'a> = Token<'a>;

// TODO: spans
// - Type
// - Stmt
// - Expr

pub struct Module<'a> {
  pub imports: Vec<Import<'a>>,
  pub symbols: Map<'a, (Vis, Symbol<'a>)>,
}

pub struct Import<'a> {
  pub path: Vec<Ident<'a>>,
  pub as_: Option<Ident<'a>>,
}

pub enum Vis {
  Public,
  Private,
}

pub enum Symbol<'a> {
  Fn(Box<Fn<'a>>),
  Class(Box<Class<'a>>),
  Trait(Box<Trait<'a>>),
  Alias(Box<Alias<'a>>),
}

pub struct Fn<'a> {
  pub name: Ident<'a>,
  pub tparams: Vec<TParam<'a>>,
  pub params: Vec<Param<'a>>,
  pub ret: Type<'a>,
  pub bounds: Vec<Bound<'a>>,
  pub body: Option<Block<'a>>,
}

pub struct TParam<'a> {
  pub name: Ident<'a>,
  pub default: Type<'a>,
}

pub struct Param<'a> {
  pub name: Ident<'a>,
  pub ty: Type<'a>,
  pub default: Option<Expr<'a>>,
}

pub enum Type<'a> {
  Name(Vec<Ident<'a>>),
  Opt(Box<Type<'a>>),
  Fn(Box<FnType<'a>>),
  Array(Box<Type<'a>>),
  Tuple(Vec<Type<'a>>),
  Generic(Box<Generic<'a>>),
}

pub struct FnType<'a> {
  pub params: Vec<Type<'a>>,
  pub ret: Type<'a>,
}

pub struct Generic<'a> {
  pub cons: Type<'a>,
  pub args: Vec<Type<'a>>,
}

pub struct Bound<'a> {
  pub ty: Type<'a>,
  pub constraint: Vec<Type<'a>>,
}

pub struct Class<'a> {
  pub name: Ident<'a>,
  pub tparams: Vec<TParam<'a>>,
  pub bounds: Vec<Bound<'a>>,
  pub members: Vec<ClassMember<'a>>,
}

pub enum ClassMember<'a> {
  Field(Ident<'a>, Type<'a>),
  Fn(Fn<'a>),
  Alias(Alias<'a>),
  Impl(Impl<'a>),
}

pub struct Trait<'a> {
  pub name: Ident<'a>,
  pub tparams: Vec<TParam<'a>>,
  pub members: Vec<TraitMember<'a>>,
}

pub enum TraitMember<'a> {
  Fn(Fn<'a>),
  Alias(Alias<'a>),
}

pub struct Alias<'a> {
  pub name: Ident<'a>,
  pub tparams: Vec<TParam<'a>>,
  pub bounds: Vec<Bound<'a>>,
  pub ty: Type<'a>,
}

pub struct Impl<'a> {
  pub tparams: Vec<TParam<'a>>,
  pub trait_: Type<'a>,
  pub members: Vec<TraitMember<'a>>,
}

pub enum Stmt<'a> {
  Var(Box<Var<'a>>),
  Loop(Box<Loop<'a>>),
  Expr(Box<Expr<'a>>),
}

pub struct Var<'a> {
  pub name: Ident<'a>,
  pub ty: Option<Type<'a>>,
  pub value: Expr<'a>,
}

pub type Block<'a> = Vec<Stmt<'a>>;

pub enum Loop<'a> {
  For(For<'a>),
  While(While<'a>),
  Inf(Inf<'a>),
}

pub struct For<'a> {
  pub item: Ident<'a>,
  pub iter: Expr<'a>,
  pub body: Block<'a>,
}

pub struct While<'a> {
  pub cond: Expr<'a>,
  pub body: Block<'a>,
}

pub struct Inf<'a> {
  pub body: Block<'a>,
}

pub enum Expr<'a> {
  Ctrl(Box<Ctrl<'a>>),
  Block(Box<Block<'a>>),
  If(Box<If<'a>>),
  Try(Box<Try<'a>>),
  Spawn(Box<Spawn<'a>>),
  Lambda(Box<Lambda<'a>>),
  Assign(Box<Assign<'a>>),
  Range(Box<Range<'a>>),
  Binary(Box<Binary<'a>>),
  Unary(Box<Unary<'a>>),
  Call(Box<Call<'a>>),
  Field(Box<Field<'a>>),
  Index(Box<Index<'a>>),
  Literal(Box<Literal<'a>>),
}

pub enum Ctrl<'a> {
  Return(Expr<'a>),
  Throw(Expr<'a>),
  Break,
  Continue,
}

pub struct If<'a> {
  pub branches: Vec<Branch<'a>>,
  pub else_: Option<Block<'a>>,
}

pub struct Branch<'a> {
  pub cond: Expr<'a>,
  pub body: Block<'a>,
}

pub struct Try<'a> {
  pub body: TryBody<'a>,
  pub branches: Vec<(Type<'a>, Block<'a>)>,
}

pub enum TryBody<'a> {
  Expr(Expr<'a>),
  Stmt(Stmt<'a>),
}

pub enum Spawn<'a> {
  Fn(Expr<'a>, Args<'a>),
  Block(Block<'a>),
}

pub struct Lambda<'a> {
  pub params: Vec<(Ident<'a>, Type<'a>)>,
  pub body: Block<'a>,
}

pub struct Assign<'a> {
  pub target: Expr<'a>,
  pub value: Expr<'a>,
}

pub struct Range<'a> {
  pub start: Expr<'a>,
  pub end: Expr<'a>,
}

pub struct Binary<'a> {
  pub op: BinaryOp,
  pub left: Expr<'a>,
  pub right: Expr<'a>,
}

pub enum BinaryOp {
  Opt,
  Or,
  And,
  Eq,
  Neq,
  More,
  Less,
  MoreEq,
  LessEq,
  Add,
  Sub,
  Mult,
  Div,
  Rem,
  Pow,
}

pub struct Unary<'a> {
  pub op: UnaryOp,
  pub inner: Expr<'a>,
}

pub enum UnaryOp {
  Neg,
  Not,
}

pub struct Call<'a> {
  pub target: Expr<'a>,
  pub args: Args<'a>,
}

pub struct Args<'a> {
  pub pos: Vec<Expr<'a>>,
  pub key: Map<'a, Expr<'a>>,
}

pub struct Field<'a> {
  pub target: Expr<'a>,
  pub name: Ident<'a>,
}

pub struct Index<'a> {
  pub target: Expr<'a>,
  pub key: Expr<'a>,
}

pub enum Literal<'a> {
  Null,
  Bool(bool),
  Int(i32),
  Float(f32),
  String(Vec<Frag<'a>>),
  Array(Array<'a>),
  Tuple(Vec<Expr<'a>>),
}

pub enum Frag<'a> {
  Str(Ident<'a>),
  Expr(Expr<'a>),
}

pub enum Array<'a> {
  Items(Vec<Expr<'a>>),
  Len(Expr<'a>, usize),
}
