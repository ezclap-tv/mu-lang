use super::*;

macro_rules! parse {
  ($src:expr) => {{
    let lexer = Lexer::new($src);
    let parser = Parser {
      lexer,
      module: Module::default(),
      errors: Vec::default(),
      ctx: Context::default(),
    };
    parser.parse()
  }};
  ($src:expr, $method:ident) => {{
    let lexer = Lexer::new($src);
    let mut parser = Parser {
      lexer,
      module: Module::default(),
      errors: Vec::default(),
      ctx: Context::default(),
    };
    parser.bump();
    match parser.$method() {
      Ok(node) => (Some(node), parser.errors),
      Err(e) => {
        parser.errors.push(e);
        (None, parser.errors)
      }
    }
  }};
}

macro_rules! snapshot {
  ($src:expr) => {
    let source = $src;
    let (module, errors) = parse!(source);
    if errors.is_empty() {
      insta::assert_debug_snapshot!(module);
    } else {
      report_to_stderr(source, errors);
    }
  };
  ($src:expr, $method:ident) => {{
    let source = $src;
    let (node, errors) = parse!(source, $method);
    if errors.is_empty() {
      insta::assert_debug_snapshot!(node.unwrap())
    } else {
      report_to_stderr(source, errors);
    }
  }};
}

fn report_to_stderr(source: &str, errors: Vec<Error>) {
  let reports = errors
    .iter()
    .map(|e| diagnosis::ToReport::to_report(e, source.into()).unwrap());
  let mut buf = String::new();
  for report in reports {
    report.emit(&mut buf).unwrap();
    eprintln!("{}", buf);
    buf.clear();
  }
  panic!("Failed to parse source, see errors above.")
}

// TODO: test for errors

#[test]
fn parse_decl_alias() {
  snapshot!("type Foo = t;");
  snapshot!("type Foo<T> = t;");
  snapshot!("type Foo<T> where T: x + y = t;");
  snapshot!("type Foo<T,> = t;");
  snapshot!("type Foo<T,> where T: x + y = t;");
  snapshot!("type Foo<T, U> = t;");
  snapshot!("type Foo<T, U> where T: x + y, U: x + y = t;");
  snapshot!("type Foo<T, U,> = t;");
  snapshot!("type Foo<T, U,> where T: x + y, U: x + y = t;");
  snapshot!("type Foo<T = d> = t;");
  snapshot!("type Foo<T = d> where T: x + y = t;");
  snapshot!("type Foo<T = d,> = t;");
  snapshot!("type Foo<T = d,> where T: x + y = t;");
  snapshot!("type Foo<T = d, U = d> = t;");
  snapshot!("type Foo<T = d, U = d> where T: x + y, U: x + y = t;");
  snapshot!("type Foo<T = d, U = d,> = t;");
  snapshot!("type Foo<T = d, U = d,> where T: x + y, U: x + y = t;");
  snapshot!("pub type Foo = t;");
}

#[test]
fn parse_decl_class() {
  snapshot!(
    "
class Foo<T,> where T: x + y {
  a: int;

  fn f();

  type U = T;

  impl<A,> Trait<A> {
    fn f();
    type V = A;
  }
}
"
  );
  snapshot!("pub class T {}");
}

#[test]
fn parse_decl_trait() {
  snapshot!(
    "
trait Foo<T,> where T: x + y {
  fn f();

  type U = T;
}
"
  );
  snapshot!("pub trait T {}");
}

#[test]
fn parse_decl_fn() {
  snapshot!("fn f();");
  snapshot!("fn f<T>(a: T, b: T, c: T) -> T where T: x + y;");
}

#[test]
fn parse_imports() {
  snapshot!("use a;");
  snapshot!("use a.b;");
  snapshot!("use a.b.c;");
  snapshot!("use a.{b, c};");
  snapshot!("use a.{b.{c}, d.{e}};");
  snapshot!("use {a.{b}, c.{d}};");

  snapshot!("use a as x;");
  snapshot!("use a.b as x;");
  snapshot!("use a.b.c as x;");
  snapshot!("use a.{b as x, c as y};");
  snapshot!("use a.{b.{c as x}, d.{e as y}};");
  snapshot!("use {a.{b as x}, c.{d as y}};");
}

#[test]
fn parse_stmt_loop() {
  snapshot!("loop {}");
  snapshot!("loop { a(); b.d() }");

  snapshot!("while a.b {}");
  snapshot!("while a.b { a(); b.d() }");

  snapshot!("for _ in 0..10 {}");
  snapshot!("for _ in 0..10 { a(); b.d() }");
}

#[test]
fn parse_stmt_let() {
  snapshot!("let v = null;");
  snapshot!("let v = 0;");
  snapshot!("let v = 0.0;");
  snapshot!("let v = true;");
  snapshot!("let v = \"test\";");
  snapshot!("let v = (v,);");
  snapshot!("let v = (v, v);");
  snapshot!("let v = [];");
  snapshot!("let v = [v, v, v, v];");
  snapshot!("let v = [v; 1024];");
  snapshot!("let v = v[0..512];");
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
fn parse_expr_block() {
  snapshot!("{ 10 }", parse_expr);
  snapshot!("{ 10; }", parse_expr);
  snapshot!("{ 10; 10 }", parse_expr);
  snapshot!("{ 10; 10; }", parse_expr);
}

#[test]
fn parse_nested_blocks() {
  snapshot!("{ { 0 } }", parse_expr);
  snapshot!("{ { 0 }; }", parse_expr);
  snapshot!("{ { 0 }; { 1 } }", parse_expr);
  snapshot!("{ { 0 }; { 1 }; }", parse_expr);
  snapshot!("{ { 0 } { 1 } }", parse_expr);
}

#[test]
fn parse_top_level_no_semi() {
  snapshot!(
    "
    { 0 }
    { 1 }
    { 2 }
    { 3 }
    "
  );
  snapshot!(
    "
    { 0 }
    if v { 1 }
    try v catch (e) { 2 }
    spawn { 3 }
    "
  );
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

  snapshot!(
    "if (T { v: if (T {}) { 0 } else { 1 } }) { 0 } else { 1 }",
    parse_expr
  );

  snapshot!("if a.b[c](d) { 0 } else { 1 }", parse_expr);
}

#[test]
fn parse_expr_try() {
  snapshot!("try {} catch (e) {}", parse_expr);

  snapshot!("try a catch (e) { e }", parse_expr);
  snapshot!("try { a } catch (e) { e }", parse_expr);
  snapshot!("try { a; } catch (e) { e }", parse_expr);
  snapshot!("try { a; b } catch (e) { e }", parse_expr);
  snapshot!("try { a; b; } catch (e) { e }", parse_expr);
  snapshot!("try a catch (e) { e; }", parse_expr);
  snapshot!("try { a } catch (e) { e; }", parse_expr);
  snapshot!("try { a; } catch (e) { e; }", parse_expr);
  snapshot!("try { a; b } catch (e) { e; }", parse_expr);
  snapshot!("try { a; b; } catch (e) { e; }", parse_expr);

  snapshot!("try a catch e { e }", parse_expr);
  snapshot!("try { a } catch e { e }", parse_expr);
  snapshot!("try { a; } catch e { e }", parse_expr);
  snapshot!("try { a; b } catch e { e }", parse_expr);
  snapshot!("try { a; b; } catch e { e }", parse_expr);
  snapshot!("try a catch e { e; }", parse_expr);
  snapshot!("try { a } catch e { e; }", parse_expr);
  snapshot!("try { a; } catch e { e; }", parse_expr);
  snapshot!("try { a; b } catch e { e; }", parse_expr);
  snapshot!("try { a; b; } catch e { e; }", parse_expr);

  snapshot!(
    "try T { v: try T {} catch (e) { e } } catch (e) { e }",
    parse_expr
  );

  snapshot!("try a.b[c](d) catch (e) { e }", parse_expr);
}

#[test]
fn parse_expr_spawn() {
  snapshot!("spawn {}", parse_expr);
  snapshot!("spawn { a; }", parse_expr);
  snapshot!("spawn { a; b }", parse_expr);
  snapshot!("spawn { a; b; }", parse_expr);

  snapshot!("spawn f()", parse_expr);
  snapshot!("spawn f(a, b, c)", parse_expr);
  snapshot!("spawn f(a, b, c,)", parse_expr);
}

#[test]
fn parse_expr_lambda() {
  snapshot!("\\_ {}", parse_expr);
  snapshot!("\\a {}", parse_expr);
  snapshot!("\\a { a }", parse_expr);
  snapshot!("\\a { a; }", parse_expr);
  snapshot!("\\a { a; b }", parse_expr);
  snapshot!("\\a { a; b; }", parse_expr);

  snapshot!("\\(a, b) {}", parse_expr);
  snapshot!("\\(a, b) { a }", parse_expr);
  snapshot!("\\(a, b) { a; }", parse_expr);
  snapshot!("\\(a, b) { a; b }", parse_expr);
  snapshot!("\\(a, b) { a; b; }", parse_expr);

  snapshot!("\\(a, b,) {}", parse_expr);
  snapshot!("\\(a, b,) { a }", parse_expr);
  snapshot!("\\(a, b,) { a; }", parse_expr);
  snapshot!("\\(a, b,) { a; b }", parse_expr);
  snapshot!("\\(a, b,) { a; b; }", parse_expr);

  snapshot!("\\{}", parse_expr);
  snapshot!("\\{ a }", parse_expr);
  snapshot!("\\{ a; }", parse_expr);
  snapshot!("\\{ a; b }", parse_expr);
  snapshot!("\\{ a; b; }", parse_expr);
}

#[test]
fn parse_expr_postfix() {
  snapshot!("a.b", parse_expr);
  snapshot!("a.b.c", parse_expr);
  snapshot!("a[b]", parse_expr);
  snapshot!("a[b][c]", parse_expr);
  snapshot!("a(b)", parse_expr);
  snapshot!("a(b)(c)", parse_expr);
  snapshot!("a.(A)", parse_expr);
  snapshot!("a.(A).(B)", parse_expr);
  snapshot!("a.b[c](d).(E)", parse_expr);

  snapshot!("a?.b", parse_expr);
  snapshot!("a?.b?.c", parse_expr);
  snapshot!("a?[b]", parse_expr);
  snapshot!("a?[b]?[c]", parse_expr);
  snapshot!("a?(b)", parse_expr);
  snapshot!("a?(b)?(c)", parse_expr);
  snapshot!("a?.(A)", parse_expr);
  snapshot!("a?.(A)?.(B)", parse_expr);
  snapshot!("a?.b?[c]?(d)?.(E)", parse_expr);

  // tuple field
  snapshot!("a.0", parse_expr);
}

#[test]
fn parse_expr_class() {
  snapshot!("Foo {}", parse_expr);
  snapshot!("Foo { a: 0 }", parse_expr);
  snapshot!("Foo { a: 0, }", parse_expr);
  snapshot!("Foo { a: 0, b: 1 }", parse_expr);
  snapshot!("Foo { a: 0, b: 1, }", parse_expr);
  snapshot!("Foo.Bar {}", parse_expr);
  snapshot!("Foo.Bar { a: 0 }", parse_expr);
  snapshot!("Foo.Bar { a: 0, }", parse_expr);
  snapshot!("Foo.Bar { a: 0, b: 1 }", parse_expr);
  snapshot!("Foo.Bar { a: 0, b: 1, }", parse_expr);
  snapshot!("Foo.Bar.<int> {}", parse_expr);
  snapshot!("Foo.Bar.<int> { a: 0 }", parse_expr);
  snapshot!("Foo.Bar.<int> { a: 0, }", parse_expr);
  snapshot!("Foo.Bar.<int> { a: 0, b: 1 }", parse_expr);
  snapshot!("Foo.Bar.<int> { a: 0, b: 1, }", parse_expr);
}

#[test]
fn parse_expr_prefix() {
  snapshot!("-a", parse_expr);
  snapshot!("!-a", parse_expr);
  snapshot!("-!-a", parse_expr);
}

#[test]
fn parse_expr_binary() {
  snapshot!("a ?? b", parse_expr);
  snapshot!("a || b", parse_expr);
  snapshot!("a && b", parse_expr);
  snapshot!("a == b != c", parse_expr);
  snapshot!("a > b >= c < d <= e", parse_expr);
  snapshot!("a - b + c", parse_expr);
  snapshot!("a * b / c % d", parse_expr);
  snapshot!("a ** b", parse_expr);
}

#[test]
fn parse_expr_range() {
  snapshot!("0..1", parse_expr);
  snapshot!("a..b", parse_expr);
  snapshot!("a+b..c+d", parse_expr);
}

#[test]
fn parse_expr_assign() {
  snapshot!("a = b", parse_expr);
  snapshot!("a = b = c", parse_expr);
  snapshot!("a += b", parse_expr);
  snapshot!("a -= b", parse_expr);
  snapshot!("a /= b", parse_expr);
  snapshot!("a *= b", parse_expr);
  snapshot!("a %= b", parse_expr);
  snapshot!("a **= b", parse_expr);
  snapshot!("a ??= b", parse_expr);
}

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
