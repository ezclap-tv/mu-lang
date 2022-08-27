// TODO: create `Spanned` type
// - the type should:
//   - auto-deref to the inner `T`
//   - transparently serialize/deserialize
//   - transparently display/debug fmt
//   - have a `Default` impl
// anything in the AST that needs a span (probably most things) will become type aliases for `Spanned<T>`.
// for example, `Expr` will be a type alias for `Spanned<ExprKind>`.
// parser should include a `.spanned` method which will wrap the Ok variant in `Spanned`
// the span should start on the previous token before parsing inner, and end on the previous token after parsing inner.
// this may require the parser to have its starting tokens (before the first bump) have spans of 0..0 instead of end..end

use serde::{Deserialize, Serialize};

#[derive(
  Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Span {
  pub start: usize,
  pub end: usize,
}

impl From<std::ops::Range<usize>> for Span {
  fn from(range: std::ops::Range<usize>) -> Self {
    Self {
      start: range.start,
      end: range.end,
    }
  }
}

#[derive(Clone, Copy, Default, Serialize, Deserialize)]
pub struct Spanned<T> {
  pub item: T,
  pub span: Span,
}

impl<T> std::ops::Deref for Spanned<T> {
  type Target = T;
  fn deref(&self) -> &Self::Target {
    &self.item
  }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.item.fmt(f)
  }
}

impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.item.fmt(f)
  }
}
