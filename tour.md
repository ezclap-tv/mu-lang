### Type System

- static: types are checked at compile-time
- structural: types are related using their properties, not identities
- strict: no null reference errors
- strong: all type conversions are explicit

### Language features

Literals (null, int, float, bool, string, f-string, tuple, array, object)

```rust
v := null
v: Null

v := 0 // int
v: Int
v := 0.0 // float
v: Float

v := true // bool
v: Bool

v := "test" // string
v := "formatted \{v}" // string interpolation
v: String

v := (v,) // tuple
v: (T,)
v := (v, v)
v: (T,T)

v := [] // array
v := [v, v, v, v]
v := [v; 1024]
v: [T]

v := { v: v, [v]: v, ...v } // object
v: { v: T }
```

Variable declaration

```rust
// inferred type
name := 0

// explicit type
// this declaration shadows the previous one, so its type may differ
name: String = "value"
```

Operators

- Arithmetic (`+`, `-`, `/`, `*`, `**`)
- Equality (`==`, `!=`)
- Comparison (`>`, `>=`, `<`, `<=`)
- Boolean (`not`/`!`, `and`/`&&`, `or`/`||`)
- Bitwise (`&`, `|`, `^`, `~`)
- Pipeline (`|>`)

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
true && true
false || true
0 & 1
0 | 1
0 ^ 1
~0
a |> b
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
if a() { /*...*/ }
elif b() { /*...*/ }
else { /*...*/ }

do { /*...*/ }

// both of the above are expressions:
// the last expression in the body is the result
v0 := if v { "a" } else { "c" }
v1 := do { "value" }
// to ensure that nothing is returned, put a semicolon after the last expression
v2: Null = do { "value"; }

// you can omit parentheses for `if` when the body is a statement
if true loop {/*...*/}
if true do {/*...*/}
if true return
if true break
// etc.
```

Loops (for, while, loop), continue/break

```rust
for item in iterator { /*...*/ }

while condition() { /*...*/ }
loop { /*...*/ }

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
  if v == 5 break
  v += 1
}

// labelled loops (applies to `for` and `while`, too)
'a: loop {
  'b: loop {
    break 'a
  }
}
```

Functions (generics, return, throw, pipeline operator)

```rust
fn name(a: A, b: B, c: C) {/*...*/}
fn name[T](a: A, b: B, c: C) -> T {/*...*/}
fn name[T](a: A, b: B, c: C) -> T throws {/*...*/}
fn name[T, E](a: A, b: B, c: C) -> T throws E {/*...*/}
fn name[T: Bound, E](a: A, b: B, c: C) -> T throws E {/*...*/}
fn name[T, E](a: A, b: B, c: C) -> T throws E where T: Bound {/*...*/}

fn name[T, E](a: A, b: B, c: C) -> T
  throws E
  where
    A: IA,
    B: IB,
    C: IC
{
  /*...*/
}

// default args
fn foo(bar: String, baz: String = "test") { /*...*/ }

fn say(greeting: String, to: String) { "\{greeting}, \{to}!" }
// positionals -> keywords
say("Hello", "world") // "Hello, world!"
say("Hello", to: "world")
say(greeting: "Hello", to: "world")
say(to: "world", greeting: "Hello")
// say(to: "world", "Hello") // invalid

fn fib(n: Int): Int {
  match n:
    ..1 -> 0
    1..=2 -> 1
    _ => fib(n-1) + fib(n+1)
}

// return may be used to return early
fn test() { return }

// `throw` may be used to return an error
// note that these are *not* exceptions, they do not unwind the stack,
// nor do they automatically propagate until caught. every error must
// be explicitly handled or propagated.
// before you can do this, the function must be declared as fallible with `!`
fn fallible0(v: Bool) throws {
  if v: throw "error-like"
}

// fallible functions collect thrown error types into an anonymous enum
fn fallible1() throws {
  if a(): throw A()
  if b(): throw B()
  if c(): throw C()
}

// the set of possible errors can also be declared explicitly
fn fallible2() throws A | B {
  if a(): throw A()
  if b(): throw B()
  if c(): throw C() // error: this function may only throw `A` or `B`
}


fn split(s: String, sep: String) -> String[] {
  out = []
  current = ""
  for ch in s {
    if ch == sep {
      out.push(current)
      current = ""
    }
    else { current += ch }
  }
  if current.len > 0 { out.push(current) }

  out
}

v := "a,b,c"
split(v, sep: ",")

// pipeline operator simplifies nested calls
print(
  [0, 1, 2, 3]
    |> map(\x {x*x})
    |> filter(\x {x == 0})
    |> sum()
)

fn square(a: Int, b: Int) -> Int { a * b }

print [1, 2, 4, 8]
  // `#` character can be used as a placeholder
  // it is only evaluated once
  |> square(#, #)
  |> sum()
```

Types, classes, enums (+ field/index access)

```rust
type Name {
  a: A,
  b: B,
}

v: Name = { a: ..., b: ... }
print(v.a)

type Name[T: Bound] { /*...*/ }
type Name[T] where T: Bound { /*...*/ }

type Any {
  fn typeid() -> TypeId,
}

// an associated type is essentially a type parameter,
// but may not differ between definitions for the same type
type AssociatedType {
  type Test: Default

  fn test() -> Self.Test {
    Self.Test.default()
  }
}

type NotAssociatedType[T] where T: Default {
  fn test() -> T { T.default() }
}

// classes are nominal types
class A[T /*: Bound*/] where T: Bound {
  // note: commas and semicolons are interchangeable and fully optional
  // the may be used to enhance readability:
  a: String, b: String, c: String

  type AssociatedType = T

  fn static_method() -> Self.AssociatedType { /*...*/ }
  // an instance method takes `self` as the first parameter
  fn instance_method(self) { /*...*/ }
}

a := A("a", "b", "c")
a := A(a: "a", b: "b", c: "c")

class Constructor {
  a, b, c: String
  rest: String[]

  fn new(parts: String[]) -> Self {
    Constructor(...parts[0..3], ...parts[3..])
  }
}

// bounds on `Self` are how you declare that you want this class to be a subtype of some `T`
class SelfBound where Self: Default {
  // you *must* implement `default` now
  fn default() -> Self { /*...*/ }
}

v := Constructor.new(["a", "b", "c", "d"])

// `Self` bounds exist here, too
enum B[T /*: Bound*/] where T: Bound {
  // again, commas and semicolons are optional
  A,
  B(T),
  C { v: T };

  type AssociatedType = T

  fn static_method() -> Self.AssociatedType { /*...*/ }
  fn instance_method(self) { /*...*/ }
}

// enums and their variants are also nominal types
enum StuffA { A, B, C }
enum StuffB { A, B, C }

fn use_stuff_a(a: StuffA) { /*...*/ }
use_stuff_a(StuffB.A) // error
use_stuff_a(StuffA.A) // ok
// may also be called like so
use_stuff_a(.A)

// since variants are types too, you can accept them directly
fn use_stuff_variant_a(a: StuffA.A) { /*...*/ }
use_stuff_variant_a(StuffA.B) // error
use_stuff_variant_a(StuffA.A) // ok
// still may be written like this
use_stuff_variant_a(.A) // ok
```

Destructuring

```rust
(a, b) = (0, 0)

type Test { a: Int, b: Int }

{ a, b } := { a: 0, b: 0 }

class Test { a: Int, b: Int }

{ a, b } := { a: 0, b: 0 }
```

Enums + match

```rust
enum Name {
  Unit
  Tuple(A, B)
  Struct { a: A, b: B }
}

// pattern matching
// it is an expression, just like `if`

// numeric
v := match v {
  0 -> /*...*/,
  1..10 -> /*...*/,
  n if n >= 10 -> /*...*/,
  // must be exhaustive
  _ -> /*...*/
}

// stringy
match v {
  "test" -> /*...*/,
  _ -> /*...*/
}

match v {
  "a" -> "b",
  "b" -> "c",
  _ -> "d"
}

// matching on types
type Test {
  a: Int
  b: Int
}

v := { a: 0, b: 0 }
v := match v:
  { a: 1..10, b: 0 } -> /*...*/,
  { a: 0, b: 1..10 } -> /*...*/,
  _ -> /*...*/

// matching on enums
v := match Name.Unit {
  .Unit -> /*...*/,
  .Tuple(a, b) -> /*...*/,
  // with nesting
  .Tuple(a: 0, b: 10) -> /*...*/,
  // and guards
  .Tuple(a: 0, b) if (0..10).contains(b) -> /*...*/,
  .Tuple(a, b) -> /*...*/,
  _ -> /*...*/
}
```

With

```rust
type Disposable {
  fn dispose(Self)
}

class File where Self: Disposable {
  fid: Int

  fn open(path: String) -> File { /* ... */ }
  fn close(self) { /* ... */ }

  fn dispose(self) { self.close() }
}

// only `Disposable`s may be used in `with`
with f := File.open("test.json") {
  use(f)
  // call to `f.dispose()` is inserted here
}
```

Errors (throw, handling, propagation, unwrapping, + match)

```rust
type A { /*...*/ }
type B { /*...*/ }

fn fallible(v: Bool) throws {
  if v: throw A
  else: throw B
}

fn fallible2() throws {
  // propagation
  v := try fallible()
  // or `try` as postfix
  /v := fallible().try

  // unwrapping
  v := try! fallible()
  v := fallible().try!
}

v := fallible() // error: must handle failure
// match on errors
v := match fallible() {
  v -> /*...*/,
  // specific error
  error A -> /*...*/,
  // specific error with binding
  error A as e -> /*...*/,
  // wildcard
  error _ -> /*...*/
}
```

Optional values

```rust
fn test[T](v: T?) -> T? {
  print(v)
}

v: String? = null
test(v) // prints "null"
v = "test"
test(v) // prints "test"
```

Modules

```rust
import module
import module.A
import module.{A, B}

import nested.module.A
import nested.module.{A, B}

import {
  a.{A, B},
  b.{C, D},
  nested.{
    d.{E, F},
    f.{G, H},
  }
}


fn something() {}
export { something }

export fn something() {}

```

Concurrency

```rust
/*
Everything is asynchronous by default.
All execution happens within the context of a task.
Whenever you do any I/O, the task is blocked until that I/O finishes,
but other tasks may be scheduled to run during during that time.

The runtime is single-threaded, which means that no synchronization,
such as atomics or locks, are ever needed.
*/

fn perform_io(label: String) {
  sleep(1)
  print("\{label} io done")
}

// first.hr
print("first a")
perform_io("first")
print("first b")

// second.hr
print("second a")
perform_io("second")
print("second b")

/*
assuming that the I/O in `first.hr` completes before `second.hr`,
running these modules would produce the following logs:

first a
second a
first io done
first b
second io done
second b

you never know when the runtime will suspend your module due to I/O,
but you also don't have to *care* about when the runtime will suspend
your module! it's easier to reason about, as all code simply looks
fully synchronous. but there are scenarios in which you explicitly
don't want to suspend execution until some I/O finishes, or you want
to perform more than one instance of I/O at the same time.
for these use cases, you can use the `spawn` keyword.
*/

// first.hr
print("first a")
spawn perform_io("first")
print("first b")

// second.hr
print("second a")
spawn perform_io("second")
print("second b")

/*
running both modules will produce the following logs:

first a
first b
second a
second b
first io done
second io done

those last two logs may switch places if `second` completes first.
*/

// you may also know tasks under names such as:
// - stackful coroutines
// - goroutines
// - fibers
// - green threads

// spawning tasks works as follows:
// arguments are evaluated, a new stack is created, the arguments
// are copied over to the new stack, and the function is called
// in the context of the new stack.
// because the runtime is single threaded, the spawned task won't begin
// executing until the current module is blocked on some I/O, or it
// itself finishes executing.
spawn task()

// spawn a "block"
// equivalent to `spawn (fn() { /*...*/ })()`
spawn { /*...*/ }

// `spawn` returns a task handle
task := spawn { /*...*/ }
do_something()
if !task.done {
  task.join() // blocks until `task` finishes executing
}

// the `async.gather` function takes in multiple task handles, and blocks
// until all of them have finished executing.
async.gather([
  spawn { /*...*/ },
  spawn { /*...*/ }
])
```
