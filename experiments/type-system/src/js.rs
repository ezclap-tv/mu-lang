use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt::Write;

use crate::{ast, builtins};

macro_rules! put {
  ($dst:expr, $($arg:tt)*) => {
    write!($dst, $($arg)*).unwrap()
  };
}

struct Context<'a> {
  scope: HashSet<Cow<'a, str>>,
  buffer: String,
}

impl<'a> Context<'a> {
  fn buf(&mut self) -> &mut String {
    &mut self.buffer
  }
}

pub fn emit(ast: ast::Program) -> String {
  let mut ctx = Context {
    scope: HashSet::new(),
    buffer: String::new(),
  };

  builtins::emit(ctx.buf());

  for expr in ast.iter() {
    emit_expr(&mut ctx, expr);
    put!(ctx.buf(), ";");
  }

  ctx.buffer
}

fn emit_expr(ctx: &mut Context, expr: &ast::Expr) {
  match &expr.item {
    ast::ExprKind::Lit(v) => emit_lit_expr(ctx, v),
    ast::ExprKind::Let(v) => emit_let_expr(ctx, v),
    ast::ExprKind::Call(v) => emit_call_expr(ctx, v),
    ast::ExprKind::Use(v) => emit_use_expr(ctx, v),
    ast::ExprKind::If(v) => emit_if_expr(ctx, v),
    ast::ExprKind::Access(v) => emit_access_expr(ctx, v),
    ast::ExprKind::Binary(v) => emit_binary_expr(ctx, v),
    ast::ExprKind::Unary(v) => emit_unary_expr(ctx, v),
  };
}

fn emit_lit_expr(ctx: &mut Context, v: &ast::Lit) {
  match v {
    ast::Lit::Number(v) => {
      put!(ctx.buf(), "{v}");
    }
    ast::Lit::Bool(v) => {
      put!(ctx.buf(), "{v}");
    }
    ast::Lit::String(v) => {
      put!(ctx.buf(), "\"{v}\"");
    }
    ast::Lit::Record(r) => {
      put!(ctx.buf(), "{{");
      for field in r {
        put!(ctx.buf(), "{}:", field.ident.lexeme);
        emit_expr(ctx, &field.value);
      }
      put!(ctx.buf(), "}}");
    }
  };
}

fn emit_let_expr(ctx: &mut Context, v: &ast::Let) {
  match &v.in_ {
    Some(in_) => {
      put!(ctx.buf(), "(()=>{{");
      match &v.kind {
        ast::LetKind::Var(v) => emit_var(ctx, v),
        ast::LetKind::Func(v) => emit_func(ctx, v),
      };
      put!(ctx.buf(), "return ");
      emit_expr(ctx, in_);
      put!(ctx.buf(), "}})()");
    }
    None => {
      match &v.kind {
        ast::LetKind::Var(v) => emit_var(ctx, v),
        ast::LetKind::Func(v) => emit_func(ctx, v),
      };
    }
  }
}

fn emit_var(ctx: &mut Context, v: &ast::Var) {
  // TODO: scoping
  put!(ctx.buf(), "let {}=", v.ident.lexeme);
  emit_expr(ctx, &v.value);
  put!(ctx.buf(), ";");
}

fn emit_func(ctx: &mut Context, v: &ast::Func) {
  // TODO: scoping
  put!(ctx.buf(), "let {}=({})=>", v.ident.lexeme, v.param.0.lexeme);
  emit_expr(ctx, &v.body);
  put!(ctx.buf(), ";");
}

fn emit_if_expr(ctx: &mut Context, v: &ast::If) {
  put!(ctx.buf(), "_if(");
  emit_expr(ctx, &v.cond);
  put!(ctx.buf(), ",()=>");
  emit_expr(ctx, &v.then);
  put!(ctx.buf(), ",()=>");
  emit_expr(ctx, &v.else_);
  put!(ctx.buf(), ")");
}

fn emit_call_expr(ctx: &mut Context, v: &ast::Call) {
  emit_expr(ctx, &v.func);
  put!(ctx.buf(), "(");
  emit_expr(ctx, &v.arg);
  put!(ctx.buf(), ")");
}

fn emit_use_expr(ctx: &mut Context, v: &ast::Use) {
  put!(ctx.buf(), "{}", v.ident.lexeme);
}

fn emit_access_expr(ctx: &mut Context, v: &ast::Access) {
  emit_expr(ctx, &v.target);
  put!(ctx.buf(), ".{}", v.field.lexeme);
}

fn emit_binary_expr(ctx: &mut Context, v: &ast::Binary) {
  put!(ctx.buf(), "(");
  emit_expr(ctx, &v.lhs);
  match v.op {
    ast::BinaryOp::Add => put!(ctx.buf(), " + "),
    ast::BinaryOp::Sub => put!(ctx.buf(), " - "),
    ast::BinaryOp::Mult => put!(ctx.buf(), " * "),
    ast::BinaryOp::Div => put!(ctx.buf(), " / "),
    ast::BinaryOp::Rem => put!(ctx.buf(), " % "),
    ast::BinaryOp::Power => put!(ctx.buf(), " ** "),
    ast::BinaryOp::And => put!(ctx.buf(), " && "),
    ast::BinaryOp::Or => put!(ctx.buf(), " || "),
    ast::BinaryOp::Equal => put!(ctx.buf(), " === "),
    ast::BinaryOp::NotEqual => put!(ctx.buf(), " !== "),
    ast::BinaryOp::LessThan => put!(ctx.buf(), " < "),
    ast::BinaryOp::LessEqual => put!(ctx.buf(), " <= "),
    ast::BinaryOp::GreaterEqual => put!(ctx.buf(), " > "),
    ast::BinaryOp::GreaterThan => put!(ctx.buf(), " >= "),
  }
  emit_expr(ctx, &v.rhs);
  put!(ctx.buf(), ")");
}

fn emit_unary_expr(ctx: &mut Context, v: &ast::Unary) {
  put!(ctx.buf(), "(");
  match v.op {
    ast::UnaryOp::Negate => put!(ctx.buf(), "-"),
  }
  emit_expr(ctx, &v.rhs);
  put!(ctx.buf(), ")");
}
