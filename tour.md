### Type System

- static: types are checked at compile-time 
- nominal: equivalence of types is defined as the equivalence of their identities
- strict: no none reference errors
- strong: all type conversions are explicit

### Language features

Literals (none, int, float, bool, string, f-string, tuple, array, map)
```rust
v = none

v = 0 // int
v = 0.0 // float

v = true // bool

v = "test" // string
v = f"formatted {v}" // f-string

v = #(v) // tuple
v = #(v, v)

v = [] // array
v = [v, v, v, v]
v = [v; 1024]

v = { v: v, [v]: v, ...v } // map
```

Variable declaration
```rust
// inferred type
name = 0

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
if a():
  /* ... */
elif b():
  /* ... */
else:
  /* ... */

do:
  /* ... */

// both of the above are expressions:
// the last expression in the body is the result
v0 = if v: "a" else: "c"
v1 = do: "value"
// to ensure that nothing is returned, put a semicolon after the last expression
v2: none = do: "value";
```

Loops (for, while, loop), continue/break
```rust
for item in iterator:
  /* body */
while condition():
  /* body */
loop :
  /* infinite loop */

for i in 0..10:
  print i

v = 10
while v >= 0:
  print v
  v -= 1

v = 0
loop:
  if v == 5: break
  v += 1

// labelled loops (applies to `for` and `while`, too)
'a: loop:
  'b: loop:
    break 'a
```

Functions (generics, return, throw, UFCS, trailing closures)
```rust
fn name(a, b, c): ...
fn name(a A, b B, c C): ...
fn name(a A, b B, c C) -> T: ...
fn name(a A, b B, c C) -> T!: ...
fn name(a A, b B, c C) -> T!E: ...

fn sum(a, b, c Int) -> Int: a + b + c

sum 1, 2, 3
sum(1, 2, 3)
sum(c: 3, b: 2, a: 1)

fn generic(v: 'T) -> 'T where 'T: Bound: v

fn fib(n):
  match n:
    ..1 -> 0
    1..=2 -> 1
    _ => (fib n-1) + (fib n-2)

// return may be used to return early
fn test(): return

// `throw` may be used to return an error
// note that these are *not* exceptions, they do not unwind the stack,
// nor do they automatically propagate until caught. every error must
// be explicitly handled or propagated.
// before you can do this, the function must be declared as fallible with `!`
fn fallible0(v: bool) -> !:
  if v: throw "error-like"

// fallible functions collect thrown error types into an anonymous enum
fn fallible1() -> !:
  if a(): throw A()
  if b(): throw B()
  if c(): throw C()

// the set of possible errors can also be declared explicitly
fn fallible2() -> ! A | B | C:
  if a(): throw A()
  if b(): throw B()
  if c(): throw C() // error: this function may only throw `A` or `B`

// UFCS
v = [0, 1, 2]
v.len
v.len()
len v
len(v)


fn split(s: string, sep: string) -> string[]:
  out: string[] = []
  current = ""
  for ch in s:
    if ch == sep:
      out.push(current)
      current = ""
    else:
      current += ch
  out.push(current)
  return out

v = "a,b,c"
v.split(",")
v.split ","
split(v, ",")
split v, ","

// pipe
v = [0, 1, 2, 3]
  map \x = x * x
  filter \x = x % 2 == 0
```

Types, traits (+ field/index access)
```rust
type Name:
  a A
  b B

v = Name(a: ..., b: ...)
print v.a

type Name[T: Bound]:
  /* ... */
type Name[T] where T: Bound:
  /* ... */

trait Any:
  fn typeid() -> TypeId

// an associated type is essentially a type parameter,
// but may not differ between definitions for the same type
trait AssociatedType:
  type Test: Default
  fn test() -> Self.Test:
    Self.Test.default()

trait NotAssociatedType[T] where T: Default:
  fn test() -> T:
    T.default()

type Test: pass

def NotAssociatedType[Int] for Test: pass
def NotAssociatedType[String] for Test: pass
Test.test() // does this return an Int or a String?

def AssociatedType for Test:
  type Test = Int
// assuming the above definitions for NotAssociatedType don't exist
// this definitely returns an Int, and we cannot define it a second time
Test.test()

v = [0, 1, 2]
print v[0]
```

Destructuring
```rust
type Test:
  a Int
  b Int

#(a, b) = #(0, 0)
a, b = #(0, 0)

Test(a, b) = Test(a: 0, b: 0)
a, b = Test(a: 0, b: 0)

```

Enums, match
```rust
enum Name:
  Tag
  TagValue (A, B)

// each variant is also a type
fn use(v: Name) -> Name.Tag { /* ... */ }

// instantiation does not require specifying enum name if it can be inferred
v: Name = .Tag // equivalent to x = Name.Tag
use(.Tag)

// pattern matching
// it is an expression, just like `if`

// numeric
v = match v:
  0 -> /*...*/
  1..10 -> /*...*/
  n if n >= 10 -> /*...*/
  // must be exhaustive
  _ -> /*...*/

// stringy
match v:
  "test" -> /*...*/
  _ -> /*...*/

match v:
  "a" -> "b",
  "b" -> "c",
  _ -> "d"

// matching on types
type Test:
  a Int
  b Int

v = Test(a: 0, b: 0)
v = match v:
  Test(a: 1..10, b: 0) -> /*...*/
  Test(a: 0, b: 1..10) -> /*...*/
  _ -> /*...*/

// matching on enums
v = match Name.Tag:
  .Tag -> /*...*/
  .TagValue (a, b) -> /*...*/
  // with nesting
  .TagValue (a: 0, b: 10) -> /*...*/
  // and guards
  .TagValue (a: 0, b) if (0..10).contains(b) -> /*...*/
  .TagValue (a, b) -> /*...*/
```

With
```rust
trait Disposable:
  fn dispose(Self)

type File:
  fid: Int

def File:
  fn open(path: string) -> File: /* ... */
  fn close(file: File): /* ... */

def Disposable for File:
  fn dispose(self): self.close()

// only `Disposable`s may be used in `with`
with File.open("test.json") as f:
  use(f)
  // call to `dispose(f)` is inserted here
```

Errors (throw, handling, propagation, unwrapping, + match)
```rust
type A: /*...*/
type B: /*...*/

fn fallible(v: bool) -> !:
  if v: throw A
  else: throw B

fn fallible2() -> !:
  // propagation
  v = fallible()?
  // unwrapping
  v = fallible()!

v = fallible() // error: must handle failure
v = match fallible(): // match on errors
  ok v -> /*...*/
  err e -> /*...*/
  
// alternatively, match specific errors right away
v = match fallible():
  Ok v -> /*...*/
  Err A -> /*...*/
  Err B -> /*...*/
```
