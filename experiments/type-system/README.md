# type-system experiment

## Overview

Core

```rust
// variables
let v = 10;

let v: int = 10; // int
let v: bool = true; /// bool
let v: str = "test"; /// str
let v: {x:int, y:int} = {x:10, y:10}; /// record
let v: {} = {} /// unit (empty record)

// functions
// - single parameter
// - parameter type is required
// - return type is optional
let fib n: int -> int =
  if n < 2 then n
  else n * fib (n - 1)
  ;

fib (v.x);
```

### Todo

- fix scoping for free variables
- convert infer to use references instead of cloning
- `and` in `let`
  - check recursive definitions for cycles without indirection
- integrate `codespan-reporting`
- record shorthand syntax (`let v = 10; {v} == {v:v}`)

- pattern matching
- tagged unions

- semicolon operator (multi-expr)
- tuples using comma operator (`print (0, 1)`, `let tuple: (int, int) = 0, 1`) + tuple types
- destructuring (`let {a, b} = {a: 0, b: 0}`)
- generics (`'t`), traits (`trait Eq = ...`), impls (`def Eq for T`, only concrete types)
- function types `T -> T`, lambdas (`fn v = v`)
- lists (`[T]`)
- nominal types (`class`)
- exceptions
- reference types + mutability
- expression chaining (side effects)
- modules
- host interop
- FFI (?)
- language server

### Development

```
$ cargo install insta
```
