use std::borrow::Cow;

use span::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Snippet<'a> {
  /// Snippet string
  pub s: Cow<'a, str>,
  /// Line number of the first line in snippet
  pub line: usize,
  /// Number of lines in this snippet
  pub count: usize,
  /// The span inside `s` which should be highlighted
  pub span: Span,
}

impl<'a> Snippet<'a> {
  pub fn new(src: &'a str, span: impl Into<Span>) -> Self {
    let span: Span = span.into();
    // the span may be multiple lines, we want to find the "full" snippet which
    // contains all the lines that the span covers.
    // for example (span is `_`):
    //   a
    //   _b
    //   cd
    //   ef_g
    //   hi
    // will yield these lines:
    //   b
    //   cd
    //   efg

    let start_line = src[..span.start].rfind('\n').unwrap_or(0);
    let end_line = src[span.end..]
      .find('\n')
      .unwrap_or_else(|| src[span.end..].len())
      + span.end;

    let s = src[start_line..end_line].trim_matches('\n');
    let line = src[..span.start].split('\n').count();
    let count = s.split('\n').count();
    let span = Span {
      start: span.start - start_line,
      end: span.end - start_line,
    };

    Self {
      s: s.into(),
      line,
      count,
      span,
    }
  }
}
