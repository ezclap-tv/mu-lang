use super::*;

macro_rules! parser {
  ($src:expr) => {{
    let lexer = Lexer::new($src);
    let mut parser = Parser {
      lexer,
      module: Module::default(),
      errors: Vec::new(),
    };
    parser.bump();
    parser
  }};
}

macro_rules! snapshot {
  ($src:expr) => {
    let mut parser = parser!($src);
    let result = parser.parse();
    insta::assert_debug_snapshot!(result);
  };
  ($src:expr, $entry:ident) => {{
    let mut parser = parser!($src);
    let result = parser.$entry();
    if !parser.errors.is_empty() {
      insta::assert_debug_snapshot!(parser.errors);
    } else {
      insta::assert_debug_snapshot!(result);
    }
  }};
}

#[test]
fn parse_type_optional() {
  snapshot!("T?", parse_type);
}

#[test]
fn parse_type_postfix() {
  snapshot!("T.A", parse_type);
  snapshot!("T[A]", parse_type);
  snapshot!("T.A[B]", parse_type);
  snapshot!("T.A[B].C", parse_type);
}

#[test]
fn parse_type_var() {
  snapshot!("T", parse_type);
}

#[test]
fn parse_type_fn() {
  snapshot!("fn()", parse_type);
  snapshot!("fn(A)", parse_type);
  snapshot!("fn(A,)", parse_type);
  snapshot!("fn(A,B)", parse_type);
  snapshot!("fn(A,B,)", parse_type);
  snapshot!("fn() -> R", parse_type);
  snapshot!("fn(A) -> R", parse_type);
  snapshot!("fn(A,) -> R", parse_type);
  snapshot!("fn(A,B) -> R", parse_type);
  snapshot!("fn(A,B,) -> R", parse_type);
}

#[test]
fn parse_type_array() {
  snapshot!("[T]", parse_type);
}

#[test]
fn parse_type_tuple() {
  snapshot!("(A,)", parse_type);
  snapshot!("(A,B)", parse_type);
  snapshot!("(A,B,)", parse_type);
  snapshot!("(A,B,C)", parse_type);
  snapshot!("(A,B,C,)", parse_type);
}

#[test]
fn parse_type_group() {
  snapshot!("(T)", parse_type);
}
