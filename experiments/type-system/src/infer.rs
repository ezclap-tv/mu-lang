//! # Type inference
//!
//! The algorithm here is known as "bidirectional type checking" or
//! "bidirectional type inference", as described in [this paper](https://davidchristiansen.dk/tutorials/bidirectional.pdf).
//! Keyword being "bidirectional". It is a simple recursive algorithm which
//! infers the types of expressions primarily from their syntax. It consists of
//! two modes, inference and checking. These two modes are implemented as two
//! functions, `infer` and `check`, which call eachother when needed.
//!
//! For example, to infer the type of an `if` expression with the following
//! syntax:
//!
//! ```ebnf
//! if_expr = "if" expr "then" expr "else" expr ;
//! ```
//!
//! We must first infer the types of all its sub-expressions (by calling
//! `infer`), then check that the type of the condition is `Bool`, and finally
//! check that the types of both branches are the same (by unifying them). If
//! all succeeds, then the type of the `if` expression is the unified type of
//! the two branches.
//!
//! Another example would be a function call:
//!
//! ```ebnf
//! call_expr = expr "(" list{expr, ","} ")" ;
//! ```
//!
//! The type of the expression is the return type of the function, but we may
//! not proceed until we confirm that the call is valid, by checking if the
//! argument types match the function's parameter types. This means first
//! checking if the number of arguments is correct, and then inferring each
//! argument's type, and checking it against the parameter type at the same
//! index.

// TODO: `infer` and `check` should also return typed AST nodes
// TODO: errors should be more granular once the language has `;` operator

use std::borrow::Cow;

use crate::error::*;
use crate::{ast, ty};

pub fn check_program<'a>(
  program: ast::Program<'a>,
  builtins: Option<ty::Bindings<'a>>,
) -> std::result::Result<ty::Bindings<'a>, Vec<Error>> {
  let mut ctx = match builtins {
    Some(builtins) => builtins.into(),
    None => ctx::Context::default(),
  };

  let mut errors = vec![];
  for expr in program {
    if let Err(e) = inner::infer(&mut ctx, expr) {
      errors.push(e);
    }
  }

  if errors.is_empty() {
    Ok(ctx.bindings())
  } else {
    Err(errors)
  }
}

mod inner {
  #![allow(clippy::needless_lifetimes)]
  use ctx::Context;

  use super::*;

  macro_rules! builtin {
    ($variant:ident) => {{
      ty::Type::Builtin(ty::Builtin::$variant)
    }};
  }

  pub fn infer<'a>(ctx: &mut Context<'a>, expr: ast::Expr<'a>) -> Result<ty::Type<'a>> {
    let expr_span = expr.span;
    match expr.item {
      ast::ExprKind::Lit(lit) => match lit {
        ast::Lit::Number(_) => Ok(builtin!(Int)),
        ast::Lit::Bool(_) => Ok(builtin!(Bool)),
        ast::Lit::String(_) => Ok(builtin!(Str)),
        ast::Lit::Record(mut ast_fields) => {
          ast_fields.sort_unstable_by_key(|f| f.ident.lexeme.clone());
          let mut ty_fields = vec![];
          for field in ast_fields {
            let ident = field.ident.lexeme.clone();
            let ty = infer(ctx, field.value)?;
            ty_fields.push(ty::Field { ident, ty });
          }
          Ok(ty::Type::Record(ty::Record { fields: ty_fields }))
        }
      },
      // TODO: type mismatch should not result in undefined var error
      ast::ExprKind::Let(kind) => {
        let (name, ty) = match kind.inner {
          ast::LetKind::Var(v) => (
            v.ident.lexeme.clone(),
            match v.type_ {
              Some(ty) => check(ctx, v.value, annotation_to_type(ty)?)?,
              None => infer(ctx, v.value)?,
            },
          ),
          ast::LetKind::Func(f) => {
            let param = (f.param.0.lexeme.clone(), annotation_to_type(f.param.1)?);
            let ret = annotation_to_type(f.ret)?;
            let ty = ty::Type::Function(Box::new(ty::Function {
              param: param.clone(),
              ret: ret.clone(),
            }));

            ctx.scope(|ctx| {
              ctx.var(f.ident.lexeme.clone(), ty.clone());
              ctx.var(param.0, param.1);
              check(ctx, f.body, ret.clone())
            })?;

            (f.ident.lexeme.clone(), ty)
          }
        };

        // TODO: `in` should be required when not in the context of an expression list
        // `(a; b; c)`
        let res = match kind.in_ {
          Some(expr) => ctx.scope(|ctx| {
            ctx.var(name.clone(), ty);
            infer(ctx, expr)
          })?,
          None => {
            ctx.var(name.clone(), ty);
            builtin!(Unit)
          }
        };
        Ok(res)
      }
      ast::ExprKind::Call(inner) => {
        let target_span = inner.func.span;
        if let ty::Type::Function(f) = infer(ctx, inner.func)? {
          check(ctx, inner.arg, f.param.1.clone())?;
          Ok(f.ret)
        } else {
          Err(Error::NotCallable { span: target_span })
        }
      }
      ast::ExprKind::Use(inner) => match ctx.get(inner.ident.lexeme.as_ref()) {
        Some(ty) => Ok(ty.clone()),
        None => Err(Error::VarNotDefined {
          token: inner.ident.into_owned(),
        }),
      },
      ast::ExprKind::If(inner) => {
        check(ctx, inner.cond, builtin!(Bool))?;
        let then_ty = infer(ctx, inner.then)?;
        check(ctx, inner.else_, then_ty.clone())?;
        Ok(then_ty)
      }
      ast::ExprKind::Access(inner) => {
        let target_span = inner.target.span;
        if let ty::Type::Record(r) = infer(ctx, inner.target)? {
          r.fields
            .iter()
            .find(|f| f.ident == inner.field.lexeme)
            .map(|f| f.ty.clone())
            .ok_or_else(|| Error::NotField {
              span: target_span,
              field: inner.field.into_owned(),
            })
        } else {
          Err(Error::NotRecord { span: target_span })
        }
      }
      ast::ExprKind::Binary(inner) => {
        macro_rules! same {
          ($ctx:ident, $inner:ident) => {{
            let ctx = $ctx;
            let inner = $inner;
            let lhs_ty = infer(ctx, inner.lhs)?;
            check(ctx, inner.rhs, lhs_ty)
          }};
        }
        macro_rules! exact {
          ($ctx:ident, $inner:ident, $span:ident, $ty:expr) => {{
            let ctx = $ctx;
            let inner = $inner;
            let span = $span;
            let ty = $ty;
            let lhs_ty = infer(ctx, inner.lhs)?;
            let rhs_ty = check(ctx, inner.rhs, lhs_ty.clone())?;
            if type_eq(&rhs_ty, &ty) {
              Ok(ty)
            } else {
              Err(Error::TypeMismatch {
                span,
                lhs: format!("{lhs_ty}"),
                rhs: format!("{rhs_ty}"),
              })
            }
          }};
        }
        macro_rules! cmp {
          ($ctx:ident, $inner:ident) => {{
            let ctx = $ctx;
            let inner = $inner;
            let lhs_ty = infer(ctx, inner.lhs)?;
            check(ctx, inner.rhs, lhs_ty)?;
            Ok(builtin!(Bool))
          }};
          ($ctx:ident, $inner:ident, $span:ident, $ty:expr) => {{
            let ctx = $ctx;
            let inner = $inner;
            let span = $span;
            let ty = $ty;
            let lhs_ty = infer(ctx, inner.lhs)?;
            let rhs_ty = check(ctx, inner.rhs, lhs_ty.clone())?;
            if type_eq(&rhs_ty, &ty) {
              Ok(builtin!(Bool))
            } else {
              Err(Error::TypeMismatch {
                span,
                lhs: format!("{lhs_ty}"),
                rhs: format!("{rhs_ty}"),
              })
            }
          }};
        }

        match inner.op {
          ast::BinaryOp::Add => same!(ctx, inner),
          ast::BinaryOp::Sub => exact!(ctx, inner, expr_span, builtin!(Int)),
          ast::BinaryOp::Mult => exact!(ctx, inner, expr_span, builtin!(Int)),
          ast::BinaryOp::Div => exact!(ctx, inner, expr_span, builtin!(Int)),
          ast::BinaryOp::Rem => exact!(ctx, inner, expr_span, builtin!(Int)),
          ast::BinaryOp::Power => exact!(ctx, inner, expr_span, builtin!(Int)),
          ast::BinaryOp::And => exact!(ctx, inner, expr_span, builtin!(Bool)),
          ast::BinaryOp::Or => exact!(ctx, inner, expr_span, builtin!(Bool)),
          ast::BinaryOp::Equal => cmp!(ctx, inner),
          ast::BinaryOp::NotEqual => cmp!(ctx, inner),
          ast::BinaryOp::LessThan => cmp!(ctx, inner, expr_span, builtin!(Int)),
          ast::BinaryOp::LessEqual => cmp!(ctx, inner, expr_span, builtin!(Int)),
          ast::BinaryOp::GreaterEqual => cmp!(ctx, inner, expr_span, builtin!(Int)),
          ast::BinaryOp::GreaterThan => cmp!(ctx, inner, expr_span, builtin!(Int)),
        }
      }
      ast::ExprKind::Unary(inner) => check(ctx, inner.rhs, builtin!(Int)),
    }
  }

  fn check<'a>(
    ctx: &mut Context<'a>,
    expr: ast::Expr<'a>,
    rhs: ty::Type<'a>,
  ) -> Result<ty::Type<'a>> {
    let expr_span = expr.span;
    let lhs = infer(ctx, expr)?;
    if type_eq(&lhs, &rhs) {
      Ok(rhs)
    } else {
      Err(Error::TypeMismatch {
        span: expr_span,
        lhs: format!("{lhs}"),
        rhs: format!("{rhs}"),
      })
    }
  }

  fn type_eq<'a>(lhs: &ty::Type<'a>, rhs: &ty::Type<'a>) -> bool {
    match (lhs, rhs) {
      (ty::Type::Builtin(ty), _) if ty == &ty::Builtin::Any => true,
      (_, ty::Type::Builtin(ty)) if ty == &ty::Builtin::Any => true,
      (ty::Type::Builtin(lhs), ty::Type::Builtin(rhs)) => lhs == rhs,
      (ty::Type::Record(lhs), ty::Type::Builtin(ty::Builtin::Unit)) => lhs.fields.is_empty(),
      (ty::Type::Builtin(ty::Builtin::Unit), ty::Type::Record(rhs)) => rhs.fields.is_empty(),
      (ty::Type::Record(lhs), ty::Type::Record(rhs)) => lhs
        .fields
        .iter()
        .zip(rhs.fields.iter())
        .all(|(lhs, rhs)| lhs.ident == rhs.ident && type_eq(&lhs.ty, &rhs.ty)),
      (ty::Type::Function(lhs), ty::Type::Function(rhs)) => {
        type_eq(&lhs.param.1, &rhs.param.1) && type_eq(&lhs.ret, &rhs.ret)
      }
      _ => false,
    }
  }

  fn annotation_to_type<'a>(ann: ast::Type<'a>) -> Result<ty::Type<'a>> {
    match ann.item {
      ast::TypeKind::Ident(v) => Ok(ty::Type::Builtin(v.lexeme.as_ref().parse().map_err(
        |_| Error::TypeNotDefined {
          token: v.into_owned(),
        },
      )?)),
      ast::TypeKind::Record(fields) => {
        let mut ty_fields = vec![];
        for (ident, ty) in fields {
          ty_fields.push(ty::Field {
            ident: ident.lexeme.clone(),
            ty: annotation_to_type(ty)?,
          });
        }
        Ok(ty::Type::Record(ty::Record { fields: ty_fields }))
      }
    }
  }
}

mod ctx {
  use indexmap::IndexMap;

  use super::*;

  pub struct Context<'a> {
    // INVARIANT: vars.len() >= 1
    vars: Vec<Vec<Var<'a>>>,
  }

  struct Var<'a> {
    name: Cow<'a, str>,
    ty: ty::Type<'a>,
  }

  impl<'a> Context<'a> {
    pub fn scope<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
      self.vars.push(vec![]);
      let res = f(self);
      self.vars.pop();
      res
    }

    pub fn var(&mut self, name: Cow<'a, str>, ty: ty::Type<'a>) {
      // SAFETY: vars.len() is always >= 1
      let stack = unsafe { self.vars.last_mut().unwrap_unchecked() };
      stack.push(Var { name, ty })
    }

    pub fn get(&mut self, name: &str) -> Option<ty::Type<'a>> {
      for stack in self.vars.iter().rev() {
        for var in stack.iter().rev() {
          if var.name.as_ref() == name {
            return Some(var.ty.clone());
          }
        }
      }
      None
    }

    pub fn bindings(self) -> ty::Bindings<'a> {
      self.vars.into()
    }
  }

  impl<'a> Default for Context<'a> {
    fn default() -> Self {
      Self { vars: vec![vec![]] }
    }
  }

  impl<'a> From<Vec<Vec<Var<'a>>>> for ty::Bindings<'a> {
    fn from(mut vars: Vec<Vec<Var<'a>>>) -> ty::Bindings<'a> {
      let mut map = IndexMap::<Cow<'a, str>, ty::Type<'a>>::default();
      for var in vars.swap_remove(0).into_iter() {
        map.insert(var.name, var.ty);
      }
      map.into()
    }
  }

  impl<'a> From<ty::Bindings<'a>> for Context<'a> {
    fn from(bindings: ty::Bindings<'a>) -> Self {
      Self {
        vars: vec![bindings
          .into_iter()
          .map(|(name, ty)| Var { name, ty })
          .collect()],
      }
    }
  }
}

/* #[cfg(test)]
mod tests {

  #[test]
  fn isolated() {
    let input = r#"
    let square n: int -> int =
      let temp =
        let v = 10 // testing the scope of this variable
      in n * n + v // ok
    ;
    "#;
    let ast = match crate::parser::parse(input) {
      Ok(ast) => ast,
      Err(errors) => {
        let mut buf = String::new();
        crate::error::report(input, &errors, &mut buf);
        panic!("{buf}");
      }
    };
    match crate::infer::check_program(ast, None) {
      Ok(bindings) => {
        println!("{bindings:?}");
      }
      Err(errors) => {
        let mut buf = String::new();
        crate::error::report(input, &errors, &mut buf);
        panic!("{buf}");
      }
    }
  }
} */
