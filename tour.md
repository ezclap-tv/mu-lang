### Type System

- static: types are checked at compile-time 
- nominal: equivalence of types is defined as the equivalence of their identities
- strict: no none reference errors
- strong: all type conversions are explicit

### Language features

Literals (none, number, bool, string, f-string, tuple, array, map)
```rust
v := none
v := 0 // int
v := 0.0 // float
v := true // bool
v := "test" // string
v := f"formatted {v}" // f-string
v := (v, v) // tuple
v := [v, v, v, v] // array
v := [v; 1024] // array
v := { v: v, [v]: v, ...v } // map
```

Variable declaration
```rust
// inferred type
name := 0

// explicit type
// this declaration shadows the previous one, so its type may differ
name: string = "value"
```

Operators
- Arithmetic (`+`, `-`, `/`, `*`, `**`)
- Equality (`==`, `!=`)
- Comparison (`>`, `>=`, `<`, `<=`)
- Boolean (`not`/`!`, `and`/`&&`, `or`/`||`)
- Bitwise (`&`, `|`, `^`, `~`)
```rust
2 + 2
2 - 2
2 / 2
2 * 2
2 ** 2
2 == 2
2 != 2
2 > 2
2 >= 2
2 < 2
2 <= 2
!true
not true
true and true
true && true
false or true
false || true
0 & 1
0 | 1
0 ^ 1
~0
```

Assignment and compound assignments
```rust
name = 1
name += 1 // desugars to `name = name + 1`
// compound assignments for all binary arithmetic and bitwise operators, and optional coalescing
name -= 1
name /= 1
name *= 1
name **= 1
name &= 1
name |= 1
name ^= 1
name ??= 1 // assigns to `name` if `name` is none
```

If, do
```rust
if a() { /* ... */ }
elif b() { /* ... */ }
else { /* ... */ }

do {
  /* ... */
}

// both of the above are expressions:
// the last expression in the body is the result
v0 := if a() { "a" } elif b() { "b" } else { "c" }
v1 := do { "value" }
// to ensure that nothing is returned, put a semicolon after the last expression
v2: none = do { "value"; }
```

Loops (for, while, loop), continue/break
```rust
for item in iterator { /* body */ }
while condition() { /* body */ }
loop { /* infinite loop */ }

for i in 0..10 {
  print i
}

v := 10
while v >= 0 {
  print v
  v -= 1
}

v := 0
loop {
  if v == 5 { break } 
  v += 1
}
```

Functions (generics, return, throw, call)
```rust
fn name(a0, a1: A, b: B, *, c: C) -> T { /* body */ }
fn generic[T](v: T) -> T { /* ... */ }
fn generic[T: InlineBound](v: T) -> T { /* ... */ }
fn generic[T](v: T) -> T
where
  T: ComplexBound
{
  /* ... */
}
// 

// last expression in the body is returned
fn identity[T](v: T) -> T { v }

// `return` maybe used to return early
fn fib(n: int) -> int {
  if (n < 1) return 0
  if (n <= 2) return 1
  return fib(n - 1) + fib(n - 2)
}

// `throw` may be used to return an error
// before you can do this, the function must be declared as fallible with `!`
fn fallible0(v: bool) -> ! {
  if v { throw "error-like" }
}
// fallible functions collect thrown error types into an anonymous enum
fn fallible1() -> ! {
  if a() { throw A() }
  if b() { throw B() }
  if C() { throw C() }
}
// the set of possible errors can also be declared explicitly
fn fallible2() -> ! A | B {
  if a() { throw A() }
  if b() { throw B() }
  if C() { throw C() } // error: this function may only throw `A` or `B`
}

// note that these are *not* exceptions, they do not unwind the stack,
// nor do they automatically propagate until caught. every error must
// be explicitly handled or propagated.
// error handling is explained in more depth later.
```

Types, type traits (+ field/index access)
```rust
type Name {
  a: A,
  b: B,
  /* ... */
}

v := Name { /* ... */ }
print v.a

type Name[T: Bound] { /* ... */ }
type Name[T] where T: Bound { /* ... */ }

type trait Name {
  // must use `Self` either in the first parameter type or the return type
  fn a(Self) -> Type
  fn b() -> Self
}

v := [0, 1, 2]
print v[0]
```

Destructuring
```rust
type Test { a: int, b: int }

a, b := (0, 0)
// names must match
a, b := Test { a: 0, b: 0 }

```

Enums, match
```rust
enum Name {
  UnitVariant,
  TupleVariant(A, B),
  FieldVariant { a: A, b: B }
}

// each variant is also a type
fn use(v: Name) -> Name.UnitVariant { /* ... */ }

// instantiation does not require specifying enum name if it can be inferred
v: Name = .UnitVariant // equivalent to x := Name.UnitVariant
use(.UnitVariant)

// pattern matching
// it is an expression, just like `if`

// numeric
v := match v {
  0 -> {/*...*/}
  1..10 -> {/*...*/}
  n if n >= 10 -> {/*...*/}
  // must be exhaustive
  _ -> {/*...*/}
}

// stringy
v := match v {
  "test" -> {/*...*/}
  _ -> {/*...*/}
}

v := match v {
  "a" -> "b",
  "b" -> "c",
  _ -> "d",
}

// matching on types
type Test { a: int, b: int }
v := Test { a: 0, b: 0 }
v := match v {
  Test { a: 1..10, b: 0 } -> {/*...*/}
  Test { a: 0, b: 1..10 } -> {/*...*/}
  _ -> {/*...*/}
}

// matching on enums
v := match Name.UnitVariant {
  .UnitVariant -> {/*...*/}
  .TupleVariant(a, b) -> {/*...*/}
  // with nesting
  .FieldVariant { a: 0, b: 10 } -> {/*...*/}
  // and guards
  .FieldVariant { a: 0, b } if (0..10).contains(b) -> {/*...*/}
  .FieldVariant { a, b } -> {/*...*/}
}
```

With
```rust
type trait Disposable {
  fn #dispose(self)
}

type File { fid: int }
fn open(path: string) -> File { /* ... */ }
fn close(file: File) { /* ... */ }

fn #dispose(self: File) { self.close() }

// only `Disposable`s may be used in `with`
with f := File.open("test.json") {
  use(f)
  // call to `#dispose(f)` is inserted here
}
```

Errors (throw, handling, propagation, unwrapping, + match)
```rust
type A {/*...*/}
type B {/*...*/}

fn fallible(v: bool) -> ! {
  if v { throw A }
  else { throw B }
}

fn fallible2() -> ! {
  // propagation
  v := fallible()?
  // unwrapping
  v := fallible()!
}

v := fallible() // error: must handle failure
v := match fallible() { // match on errors
  ok v -> {/*...*/}
  err e -> {/*...*/}
}
// alternatively, match specific errors right away
v := match fallible() {
  ok v -> {/*...*/}
  err A { ... } -> {/*...*/}
  err B { ... } -> {/*...*/}
}
```