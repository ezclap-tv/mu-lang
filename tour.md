### Type System
- static: types are checked at compile-time 
- nominal: equivalence of types is defined as the equivalence of their identities
- strict: no null reference errors
- strong: all type conversions are explicit

### Syntax + Behavior

- Variables
- Assignments (+ compound)
- If, do
- Loops (for, while, loop), continue/break
- Functions (generics, return, throw, call)
- Classes, class traits
- Enums
- Type aliases
- Match
- With
- Destructuring
- Field existence (`in`)
- Field access
- Index access
- Arithmetic (`+`, `-`, `/`, `*`, `**`)
- Equality (`==`, `!=`)
- Comparison (`>`, `>=`, `<`, `<=`)
- Boolean (`not`/`!`, `and`/`&&`, `or`/`||`)
- Bitwise (`&`, `|`, `^`, `~`)
- Optional coalescing (field, index, call)
- Pipe
- Literals (null, number, bool, string, f-string, symbol, record, tuple, array)
- Error propagation
- Error unwrapping

Variable declaration
```swift
// inferred type
name := "value"

// explicit type
name: string = "value"
```

Assignment and compound assignments
```swift
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
name ??= 1 // assigns to `name` if `name` is null
```

If, do
```swift
if a() { /* ... */ }
else if b() { /* ... */ }
else { /* ... */ }

do {
  /* ... */
}

// both of the above are expressions:
// the last expression in the body is the result
v0 := if a() { "a" }
     else if b() { "b" }
     else { "c" }
v1 := do { "value" }
// to ensure that nothing is returned, put a semicolon after the last expression
v2: null = do { "value"; }
```

Loops (for, while, loop), continue/break
```swift
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

Functions, generics, 
```swift
fn name(a0, a1: A, b: B, *, c: C) -> T { /* body */ }
fn generic[T](v: T) -> T { /* ... */ }
fn generic[T: InlineBound](v: T) -> T { /* ... */ }
fn generic[T](v: T) -> T
requires
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

