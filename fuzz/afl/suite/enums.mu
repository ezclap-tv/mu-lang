num Name {
  Unit
  Simple(T)
  Tuple(A, B)
  Struct { a: A, b: B }
}

class Test {
  a: string
}

// pattern matching
// it is an expression, just like `if`

// numeric
v := match v {
  0 => /*...*/,
  1..10 => /*...*/,
  n if n >= 10 => /*...*/,
  // must be exhaustive
  _ => /*...*/
}

// stringy
match v {
  "test" => /*...*/,
  _ => /*...*/
}

match v {
  "a" => "b",
  "b" => "c",
  _ => "d"
}

// matching on types
type Test {
  a: int
  b: int
}

v := { a: 0, b: 0 }
v := match v {
  { a: 1..10, b: 0 } => /*...*/,
  { a: 0, b: 1..10 } => /*...*/,
  _ => /*...*/
}

// matching on enums
v := match Name.Unit {
  .Unit => /*...*/,
  .Tuple(a, b) => /*...*/,
  // with nesting
  .Tuple(a: 0, b: 10) => /*...*/,
  // and guards
  .Tuple(a: 0, b) if (0..10).contains(b) => /*...*/,
  .Tuple(a, b) => /*...*/,
  _ => /*...*/
}

// postfix match
v := a.match { /*...*/ }

// record matching
v := match { a: /* some string */ } {
  { a: "test" } => /*...*/
  { a } => /*...*/
}
// class matching - equivalent to record matching
v := match Test(a: /* some string */) {
  { a: "test" } => /*...*/
  { a } => /*...*/
}