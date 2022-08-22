version 1: no generics, no nullability, no nominal types

```rust
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
  print n + " * " + n + " = " + r
  ;
```
