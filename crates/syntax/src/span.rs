use std::ops::{Deref, DerefMut, Range};

use serde::{Deserialize, Serialize};

#[derive(
  Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Span {
  pub start: usize,
  pub end: usize,
}

impl From<Range<usize>> for Span {
  fn from(value: Range<usize>) -> Self {
    Self {
      start: value.start,
      end: value.end,
    }
  }
}

impl std::fmt::Display for Span {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}..{}", self.start, self.end)
  }
}

#[derive(Clone, Copy, Default, Serialize, Deserialize)]
pub struct Spanned<T> {
  value: T,
  pub span: Span,
}

impl<T> Spanned<T> {
  pub fn new(value: T, span: impl Into<Span>) -> Spanned<T> {
    Spanned {
      value,
      span: span.into(),
    }
  }

  pub fn into_inner(self) -> T {
    self.value
  }
}

impl<T> Deref for Spanned<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl<T> DerefMut for Spanned<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.value
  }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.value.fmt(f)
  }
}

impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.value.fmt(f)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[allow(clippy::no_effect)]
  #[test]
  fn test_spanned() {
    #[derive(Default)]
    struct Nested {
      v: i32,
    }
    #[derive(Default)]
    struct Test {
      a: i32,
      b: i32,
      c: i32,
      nested: Nested,
    }

    let mut t = Spanned::new(Test::default(), 0..10);

    t.span.start;
    t.span.end;
    t.a;
    t.b;
    t.c;
    t.nested.v = 10;
  }
}
