use super::{Report, Snippet};
use crate::report::{Level, Source};

#[test]
fn snippet_single_line() {
  let src = "lorem ipsum dolor sit amet consectetur adipiscing elit";

  assert_eq!(
    Snippet::new(src, 6..17),
    Snippet {
      s: "lorem ipsum dolor sit amet consectetur adipiscing elit".into(),
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
      s: "lorem ipsum\ndolor sit amet".into(),
      line: 1,
      count: 2,
      span: (6..17).into(),
    }
  );

  let src = "lorem ipsum\ndolor sit amet\nconsectetur adipiscing elit";

  assert_eq!(
    Snippet::new(src, 17..31),
    Snippet {
      s: "dolor sit amet\nconsectetur adipiscing elit".into(),
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
    source: Source::file("test.mu", "let x = 10\nlet y = 20;"),
    message: "expected semicolon".into(),
    span: (10..11).into(),
    label: None,
    color: true,
  };
  insta::assert_snapshot!(report.emit_to_string().unwrap());
}

#[test]
fn emit_report_multi_line() {
  let report = Report {
    level: Level::Error,
    source: Source::file("test.mu", "let x: Foo = Bar {\n  a: 0,\n  b: 0,\n};"),
    message: "mismatched type".into(),
    span: (13..36).into(),
    label: Some("expected `Foo`, found `Bar`".into()),
    color: true,
  };
  insta::assert_snapshot!(report.emit_to_string().unwrap());
}

#[test]
fn emit_report_multi_line_large() {
  let report = Report {
    level: Level::Error,
    source: Source::file(
      "test",
      "let x: Foo = Bar {\n  a: 0,\n  b: 0,\n  c: 0,\n  d: 0,\n  e: 0,\n  f: 0,\n  g: 0,\n};",
    ),
    message: "mismatched type".into(),
    span: (13..76).into(),
    label: Some("expected `Foo`, found `Bar`".into()),
    color: true,
  };
  insta::assert_snapshot!(report.emit_to_string().unwrap());
}

#[test]
fn emit_report_single_line_no_color() {
  let report = Report {
    level: Level::Error,
    source: Source::file("test.mu", "let x = 10\nlet y = 20;"),
    message: "expected semicolon".into(),
    span: (10..11).into(),
    label: None,
    color: false,
  };
  insta::assert_snapshot!(report.emit_to_string().unwrap());
}

#[test]
fn emit_report_multi_line_no_color() {
  let report = Report {
    level: Level::Error,
    source: Source::file("test.mu", "let x: Foo = Bar {\n  a: 0,\n  b: 0,\n};"),
    message: "mismatched type".into(),
    span: (13..36).into(),
    label: Some("expected `Foo`, found `Bar`".into()),
    color: false,
  };
  insta::assert_snapshot!(report.emit_to_string().unwrap());
}

#[test]
fn emit_report_multi_line_large_no_color() {
  let report = Report {
    level: Level::Error,
    source: Source::file(
      "test.mu",
      "let x: Foo = Bar {\n  a: 0,\n  b: 0,\n  c: 0,\n  d: 0,\n  e: 0,\n  f: 0,\n  g: 0,\n};",
    ),
    message: "mismatched type".into(),
    span: (13..76).into(),
    label: Some("expected `Foo`, found `Bar`".into()),
    color: false,
  };
  insta::assert_snapshot!(report.emit_to_string().unwrap());
}
