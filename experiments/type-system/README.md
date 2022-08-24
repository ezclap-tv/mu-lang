# type-system experiment

## Overview

Core

```rust
// variables
let v = 10;

let v: int = 10; // int
let v: bool = true; /// bool
let v: str = "test"; /// str
let v: {x:int, y:int} = {x: 10, y: 10}; /// record
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

### Roadmap

mvp

- function calls with records as arguments should not require parentheses (`print {x:10}`)
- `=` instead of `:` for record fields
- type checker
  - structural type comparison
  - unidirectional type inference
- emit javascript

post-mvp

- function types `T -> T`
- optional return types
- lambdas (`fn v = v`)
- tuples using comma operator (`print (0, 1)`, `let tuple: (int, int) = 0, 1`) + tuple types
- grouping type (`(int -> int) -> int`, `int -> (int -> int)`)
- lists
- nominal types (`class`)
- tagged unions + pattern matching
- reference types + mutability
- expression chaining (side effects)
- generics, traits, impls
- modules
- host interop
- FFI (?)
- language server

### Development

```
$ cargo install insta
```