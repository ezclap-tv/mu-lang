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
fn parse_expr_literal() {
  snapshot!("null", parse_expr);
  snapshot!("100", parse_expr);
  snapshot!("3.14", parse_expr);
  snapshot!("true", parse_expr);
  snapshot!("false", parse_expr);
  snapshot!("\"test\"", parse_expr);
  snapshot!("\"test \\n\"", parse_expr);
}

#[test]
fn parse_expr_array() {
  snapshot!("[0, 1, 2]", parse_expr);
  snapshot!("[0; 100]", parse_expr);
}

#[test]
fn parse_expr_tuple() {
  snapshot!("(0,)", parse_expr);
  snapshot!("(0,1)", parse_expr);
  snapshot!("(0,1,)", parse_expr);
  snapshot!("(0,1,2)", parse_expr);
  snapshot!("(0,1,2,)", parse_expr);
}

#[test]
fn parse_expr_group() {
  snapshot!("(0)", parse_expr);
}

#[test]
fn parse_expr_var() {
  snapshot!("test", parse_expr);
}

#[test]
fn parse_expr_ctrl() {
  snapshot!("return", parse_expr);
  snapshot!("return 10", parse_expr);
  snapshot!("throw", parse_expr);
  snapshot!("throw 10", parse_expr);
  snapshot!("continue", parse_expr);
  snapshot!("break", parse_expr);
}

#[test]
fn parse_expr_do() {
  snapshot!("do { 10 }", parse_expr);
  snapshot!("do { 10; }", parse_expr);
  snapshot!("do { 10; 10 }", parse_expr);
  snapshot!("do { 10; 10; }", parse_expr);
}

#[test]
fn parse_expr_if() {
  snapshot!("if true { a } else { a }", parse_expr);
  snapshot!("if true { a } else if false { a } else { a }", parse_expr);
  snapshot!("if true { a; b } else { a; b }", parse_expr);
  snapshot!(
    "if true { a; b } else if false { a; b } else { a; b }",
    parse_expr
  );
  snapshot!("if true { a; b; } else { a; b; }", parse_expr);
  snapshot!(
    "if true { a; b; } else if false { a; b; } else { a; b; }",
    parse_expr
  );
}

// TODO: once the above is fixed, continue with expr testing:
// (if, try, spawn, lambda) + everything above expr_primary

/* #[test]
fn parse_expr_assign() {
  snapshot!("a = b", parse_expr);
  snapshot!("a = b = c", parse_expr);
  snapshot!("a = b = c", parse_expr);
} */

#[test]
fn parse_type_optional() {
  snapshot!("T?", parse_type);
}

#[test]
fn parse_type_path() {
  snapshot!("T", parse_type);
  snapshot!("T.A", parse_type);
  snapshot!("T[A]", parse_type);
  snapshot!("T.A[B]", parse_type);
  snapshot!("T.A[B].C", parse_type);
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
