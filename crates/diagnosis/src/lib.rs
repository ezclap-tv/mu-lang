use std::fmt;
use std::fmt::Write;
use std::ops::Range;

use owo_colors::{AnsiColors, DynColors, OwoColorize};
use span::Span;
use thiserror::Error;

pub enum Level {
  Info,
  Warning,
  Error,
}

pub struct File<'a> {
  pub name: &'a str,
  pub source: &'a str,
}

pub struct Report<'a> {
  pub level: Level,
  pub file: File<'a>,
  pub message: &'a str,
  pub span: Span,
  pub label: Option<&'a str>,
}

impl<'a> Report<'a> {
  pub fn emit<W: Write>(self, w: &mut W) -> Result<()> {
    // examples:

    // single line span
    // error: expected semicolon
    // > foo.mu:11
    // |
    // | let y = 30_
    // |

    // multi-line span
    // error: mismatched type
    // > bar.mu:31
    // |
    // | let x: Foo = Bar {
    // |   a: 0,
    // |   b: 0,
    // | }
    // |
    // + expected "Foo", found "Bar"

    // with help
    // error: undefined variable
    // > baz.mu:12
    // |
    // | y = 10;
    // |

    // TODO: add a case for VERY LARGE multi-line snippets,
    //       where they are truncated to a max of 5 lines
    // TODO: clean this up a bit

    if self.file.source.get(Range::from(self.span)).is_none() {
      return Err(Error::OutOfBounds);
    }

    let snippet = Snippet::new(self.file.source, self.span);
    let color = match self.level {
      Level::Info => DynColors::Ansi(AnsiColors::Blue),
      Level::Warning => DynColors::Ansi(AnsiColors::Yellow),
      Level::Error => DynColors::Ansi(AnsiColors::Red),
    };

    // {level}: {message}
    match self.level {
      Level::Info => write!(w, "{}", "info".blue())?,
      Level::Warning => write!(w, "{}", "warning".yellow())?,
      Level::Error => write!(w, "{}", "error".red())?,
    }
    writeln!(w, ": {}", self.message)?;
    // > {file.name}:{line}
    writeln!(w, "{} {}:{}", ">".blue(), self.file.name, snippet.line)?;

    writeln!(w, "{} ", "|".blue())?;
    if snippet.s[Range::from(snippet.span)].trim().is_empty() {
      // "whitespace-only" snippet, may happen in case the span is 1 character wide
      // and lands on a newline.
      let n = snippet.s[Range::from(snippet.span)].len();
      writeln!(
        w,
        "{} {}{}",
        "|".blue(),
        &snippet.s[..snippet.span.start],
        format!("{:_<width$}", "_", width = n)
          .color(color)
          .underline(),
      )?;
    } else if snippet.count > 1 {
      // multi-line snippet
      let first_lf = snippet.s[snippet.span.start..]
        .find('\n')
        .map(|i| i + snippet.span.start)
        .unwrap_or(snippet.s.len());
      let last_lf = snippet.s[snippet.span.start..snippet.span.end]
        .rfind('\n')
        .map(|i| i + snippet.span.start)
        .unwrap_or(snippet.s.len());
      writeln!(
        w,
        "{} {}{}",
        "|".blue(),
        &snippet.s[..snippet.span.start],
        snippet.s[snippet.span.start..first_lf]
          .trim()
          .color(color)
          .underline()
      )?;
      if snippet.count > 2 {
        for line in snippet.s[first_lf..last_lf].split('\n').skip(1) {
          writeln!(w, "{} {}", "|".blue(), line.color(color).underline())?;
        }
      }
      writeln!(
        w,
        "{} {}{}",
        "|".blue(),
        snippet.s[last_lf..snippet.span.end]
          .trim()
          .color(color)
          .underline(),
        &snippet.s[snippet.span.end..],
      )?;
    } else {
      // single-line snippet
      writeln!(
        w,
        "{} {}{}{}",
        "|".blue(),
        &snippet.s[..snippet.span.start],
        (&snippet.s[Range::from(snippet.span)])
          .color(color)
          .underline(),
        &snippet.s[snippet.span.end..]
      )?;
    }
    writeln!(w, "{} ", "|".blue())?;
    if let Some(label) = self.label {
      writeln!(w, "{} {}", "+".blue(), label)?;
    }

    Ok(())
  }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Snippet<'a> {
  /// Snippet string
  pub s: &'a str,
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
      s,
      line,
      count,
      span,
    }
  }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
  #[error("failed to format")]
  Fmt(#[from] fmt::Error),
  #[error("span does not fit within source string")]
  OutOfBounds,
}

#[cfg(test)]
mod tests {
  use super::{Report, Snippet};
  use crate::{File, Level};

  #[test]
  fn snippet_single_line() {
    let src = "lorem ipsum dolor sit amet consectetur adipiscing elit";

    assert_eq!(
      Snippet::new(src, 6..17),
      Snippet {
        s: "lorem ipsum dolor sit amet consectetur adipiscing elit",
        line: 1,
        count: 1,
        span: (6..17).into(),
      }
    );
  }

  #[test]
  fn snippet_multi_line() {
    let src = "lorem ipsum\ndolor sit amet\nconsectetur adipiscing elit";

    assert_eq!(
      Snippet::new(src, 6..17),
      Snippet {
        s: "lorem ipsum\ndolor sit amet",
        line: 1,
        count: 2,
        span: (6..17).into(),
      }
    );

    let src = "lorem ipsum\ndolor sit amet\nconsectetur adipiscing elit";

    assert_eq!(
      Snippet::new(src, 17..31),
      Snippet {
        s: "dolor sit amet\nconsectetur adipiscing elit",
        line: 2,
        count: 2,
        span: (6..20).into(),
      }
    );
  }

  #[test]
  fn emit_report_single_line() {
    let report = Report {
      level: Level::Error,
      file: File {
        name: "test.mu",
        source: "let x = 10\nlet y = 20;",
      },
      message: "expected semicolon",
      span: (10..11).into(),
      label: None,
    };

    let mut buf = String::new();

    report.emit(&mut buf).unwrap();

    insta::assert_snapshot!(buf);
  }

  #[test]
  fn emit_report_multi_line() {
    let report = Report {
      level: Level::Error,
      file: File {
        name: "test.mu",
        source: "let x: Foo = Bar {\n  a: 0,\n  b: 0,\n};",
      },
      message: "mismatched type",
      span: (13..36).into(),
      label: Some("expected `Foo`, found `Bar`"),
    };

    let mut buf = String::new();

    report.emit(&mut buf).unwrap();

    insta::assert_snapshot!(buf);
  }
}
