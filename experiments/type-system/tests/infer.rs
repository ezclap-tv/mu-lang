use compiler::*;

macro_rules! assert_ok {
  ($name:ident, { $($key:literal => $ty:expr),* }, $source:literal) => {
    #[test]
    fn $name() {
      let source = $source;
      let name = stringify!($name);
      let builtins = [$((std::borrow::Cow::from($key), $ty)),*].into_iter().collect();
      let ast = match parser::parse(&source) {
        Ok(ast) => ast,
        Err(errors) => {
          let mut buf = String::new();
          error::report(&source, &errors, &mut buf);
          panic!("{name} should have parsed sucessfully, but it failed:\n{buf}");
        }
      };
      match infer::check_program(ast, Some(builtins)) {
        Ok(bindings) => {
          insta::assert_yaml_snapshot!(bindings);
        }
        Err(errors) => {
          let mut buf = String::new();
          error::report(&source, &errors, &mut buf);
          panic!("{name} should have type checked successfully, but it failed:\n{buf}");
        }
      }
    }
  };
}

macro_rules! assert_error {
  ($name:ident, { $($key:literal => $ty:expr),* }, $source:literal) => {
    #[test]
    fn $name() {
      let source = $source;
      let name = stringify!($name);
      let builtins = [$((std::borrow::Cow::from($key), $ty)),*].into_iter().collect();
      let ast = match parser::parse(&source) {
        Ok(ast) => ast,
        Err(errors) => {
          let mut buf = String::new();
          error::report(&source, &errors, &mut buf);
          panic!("{name} should have parsed sucessfully, but it failed:\n{buf}");
        }
      };
      match infer::check_program(ast, Some(builtins)) {
        Ok(_) => {
          panic!("{name} should have failed to type check, but it was successful");
        }
        Err(errors) => {
          insta::assert_yaml_snapshot!(errors);
        }
      }
    }
  };
}

assert_ok!(
  if_expr,
  {},
  r#"
  if true then 1 else 0;
  "#
);

assert_error!(
  if_expr_with_non_bool_cond,
  {},
  r#"
  if 0 then 1 else 0;
  "#
);

assert_ok!(
  variable,
  {},
  r#"
  let x = if true then 1 else 0;
  "#
);

assert_ok!(
  call,
  {},
  r#"
  let f a: int -> int = a;
  f(0);
  "#
);

assert_ok!(
  full,
  {
    "intToStr" => ty::Type::Function(Box::new(ty::Function {
      param: ("n".into(), ty::Type::Builtin(ty::Builtin::Int)),
      ret: ty::Type::Builtin(ty::Builtin::Str)
    })),
    "print" => ty::Type::Function(Box::new(ty::Function {
      param: ("v".into(), ty::Type::Builtin(ty::Builtin::Any)),
      ret: ty::Type::Builtin(ty::Builtin::Any)
    }))
  },
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
    let ns = intToStr (n) in
    print (ns + " * " + ns + " = " + r)
    ;

  let unwrap r: {v:int} -> int = r.v;

  print (unwrap {v:10});
  "#
);

assert_ok!(
  shadowing,
  {
    "intToStr" => ty::Type::Function(Box::new(ty::Function {
      param: ("n".into(), ty::Type::Builtin(ty::Builtin::Int)),
      ret: ty::Type::Builtin(ty::Builtin::Str)
    })),
    "print" => ty::Type::Function(Box::new(ty::Function {
      param: ("v".into(), ty::Type::Builtin(ty::Builtin::Any)),
      ret: ty::Type::Builtin(ty::Builtin::Any)
    }))
  },
  r#"
  let print_square n: int -> int =
    let sq = intToStr (n * n) in
    // shadows parameter with type `str`
    let n = intToStr (n) in
    // `+` only checks that all operands have the same type, in this case `str`
    print (n + " * " + n + " = " + sq)
    ;
  "#
);

assert_error!(
  scoping,
  {
    "print" => ty::Type::Function(Box::new(ty::Function {
      param: ("v".into(), ty::Type::Builtin(ty::Builtin::Any)),
      ret: ty::Type::Builtin(ty::Builtin::Any)
    }))
  },
  r#"
  let foo v: {} -> {} = let x = 10;
  print (x);
  "#
);
