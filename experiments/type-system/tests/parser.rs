use compiler::*;

macro_rules! assert_ok {
  ($name:ident $source:literal) => {
    #[test]
    fn $name() {
      let source = $source;
      let name = stringify!($name);
      match parser::parse(&source) {
        Ok(ast) => {
          insta::assert_yaml_snapshot!(ast);
        }
        Err(errors) => {
          let mut buf = String::new();
          error::report(&source, &errors, &mut buf);
          panic!("{name} should have parsed sucessfully, but it failed:\n{buf}");
        }
      }
    }
  };
}

macro_rules! assert_error {
  ($name:ident $source:literal) => {
    #[test]
    fn $name() {
      let source = $source;
      let name = stringify!($name);
      match parser::parse(&source) {
        Ok(_) => {
          panic!("{name} should have failed to parse, but it was successful");
        }
        Err(errors) => {
          insta::assert_yaml_snapshot!(errors);
        }
      }
    }
  };
}

assert_ok!(full
r#"
// variables
let v = 10;

let v: int = 10; // int
let v: bool = true; /// bool
let v: str = "test"; /// str
let v: {x:int, y:int} = {x: 10, y: 10}; /// record

// functions
// single argument
// parameter and return type annotations are required
let fib n: int -> int =
  if n < 2 then n
  else n * fib (n - 1)
  ;

// function calls
fib (v.x);

let print_square n: int -> int =
  let r = intToStr (n * n) in
  print (n + " * " + n + " = " + r)
  ;

let unwrap r: {v:int} -> int = r.v;

print (unwrap {v:10});
"#);

assert_ok!(simple_expr
r#"
v || v;
v && v;
v == v;
v != v;
v < v;
v > v;
v <= v;
v >= v;
v + v;
v - v;
v * v;
v / v;
v % v;
v ** v;
~v;
~~v;
~~~~~~~~~v;
~~~~~~~~~~v;
v.f;
f (v);
f {v:v};
v.f (v);
v.f {v:v};
10;
true;
false;
"test";
{v:v};
v;
(((v)));
"#);

assert_error!(top_level_without_semi
r#"
let x = 10
let y = 5
"#
);

assert_ok!(types
r#"
let v: T = v;
let v: {v: T} = v;
let v: {v: T, v: T} = v;
let v: (((T))) = v;
"#);
