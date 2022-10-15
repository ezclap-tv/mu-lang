#[macro_export]
/// Creates an AST binary expression
macro_rules! binary {
  ($self:ident, $left:ident, $right:expr) => {{
    let token = $self.previous.clone();
    let l = $left;
    let r = $right;
    let span = l.span.start..r.span.end;

    BinOp::new(token, l, r).map(|op| Expr::new(ExprKind::BinOp(Box::new(op)), span))
  }};
}

#[macro_export]
macro_rules! unary {
  ($operand:expr, $op:path, $token:ident) => {{
    let operand = $operand;
    let span = $token.span.start..operand.span.end;

    Expr::new(
      ExprKind::UnOp(Box::new(UnOp {
        token: $token,
        kind: $op,
        operand,
      })),
      span,
    )
  }};
}

#[macro_export]
macro_rules! try_expr {
  (prefix => $operand:expr, $op:path, $token:ident) => {
    try_expr!(is_postfix = false | $operand, $op, $token)
  };

  (postfix => $operand:expr, $op:path, $token:ident) => {
    try_expr!(is_postfix = true | $operand, $op, $token)
  };

  (is_postfix = $is:tt | $operand:expr, $op:path, $token:ident) => {{
    let operand = $operand;
    let span = $token.span.start..operand.span.end;

    Expr::new(
      ExprKind::Try(Box::new(TryExpr {
        token: $token,
        kind: $op,
        is_postfix: $is,
        operand,
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
      ExprKind::PrimitiveLiteral(Box::new(PrimitiveLiteral { token, value })),
      span,
    )
  }};
}

#[macro_export]
macro_rules! assignment {
  (if $self:ident $matched:ident = [ $($op:ident),* ] => $block:expr) => {{
    let _matched = match &$self.current.kind {
      $(
        TokenKind::$op => Some(mu_ast::AssignmentKind::$op),
      )*
      _ => None
    };
    if let Some($matched) = _matched {
      $self.advance();
      $block
    }
  }};
}
