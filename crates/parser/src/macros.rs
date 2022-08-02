#[macro_export]
/// Creates an AST binary expression
macro_rules! binary {
  ($self:ident, $left:ident, $right:expr) => {{
    let token = $self.previous.clone();
    let l = $left;
    let r = $right;
    let span = l.span.start..r.span.end;

    BinOp::new(token, l, r).map(|op| Expr::new(ExprKind::BinOp($self.arena.boxed(op)), span))
  }};
}

#[macro_export]
macro_rules! unary {
  ($self:ident, $operand:expr, $op:path, $token:ident) => {{
    let operand = $operand;
    let span = $token.span.start..operand.span.end;

    Expr::new(
      ExprKind::UnOp($self.arena.boxed(UnOp {
        token: $token,
        kind: $op,
        operand: operand,
      })),
      span,
    )
  }};
}

#[macro_export]
macro_rules! try_expr {
  (prefix => $self:ident, $operand:expr, $op:path, $token:ident) => {
    try_expr!(is_postfix = false | $self, $operand, $op, $token)
  };

  (postfix => $self:ident, $operand:expr, $op:path, $token:ident) => {
    try_expr!(is_postfix = true | $self, $operand, $op, $token)
  };

  (is_postfix = $is:tt | $self:ident, $operand:expr, $op:path, $token:ident) => {{
    let operand = $operand;
    let span = $token.span.start..operand.span.end;

    Expr::new(
      ExprKind::Try($self.arena.boxed(TryExpr {
        token: $token,
        kind: $op,
        is_postfix: $is,
        operand: operand,
      })),
      span,
    )
  }};
}

#[macro_export]
macro_rules! literal {
  ($self:ident, $value:expr) => {{
    let token = $self.previous.clone();
    let span = token.span.clone();
    let value = $value;
    Expr::new(
      ExprKind::PrimitiveLiteral($self.arena.boxed(PrimitiveLiteral { token, value })),
      span,
    )
  }};
}
