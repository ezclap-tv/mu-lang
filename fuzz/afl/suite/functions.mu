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

// anonymous function
square := \x {x*x}

// positionals, defaults, rest, named + call examples
fn foo(a, b, c: string, d = "test", rest: string..., named: string = "named") {
  print(a, b, c, d, rest, named)
}
foo("a", "b", "c") // a b c test [] named
foo("a", "b", "c", "d", "f", "g", "h") // a b c d [f g h] named
foo("a", "b", "c", "d", "f", "g", "h", named: "i") // a b c d [f g h] i
foo("a", b: "b", "c") // error: `c` must be labelled

fn bar(..., named_only: string) {
  print(named_only)
}
bar("a") // error: function accepts no positional args
bar(named_only: "a") // a

fn fib(n: int): int {
  match n {
    ..1 => 0
    1..=2 => 1
    _ => fib(n-1) + fib(n+1)
  }
}

// return may be used to return early
fn test() { return }

// `throw` may be used to return an error
// note that these are *not* exceptions, they do not unwind the stack,
// nor do they automatically propagate until caught. every error must
// be explicitly handled or propagated.
// before you can do this, the function must be declared as fallible with `throws`
fn fallible0(v: bool) throws {
  if v { throw "error-like" }
}

// fallible functions collect thrown error types into an anonymous enum
fn fallible1() throws {
  if a() { throw A() }
  if b() { throw B() }
  if c() { throw C() }
}

// the set of possible errors can also be declared explicitly
fn fallible2() throws A | B {
  if a() { throw A() }
  if b() { throw B() }
  if c() { throw C() } // error: this function may only throw `A` or `B`
}


fn split(s: string, sep: string) -> [string] {
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
  // if the function has positional arity of 1, and no required named params,
  // then the call can be omitted
  //
  // |> sum
  //
  // this is useful when you want to use pipeline with lambdas
  //
  // 10 |> \x {x*x}
  //
  // the above is equivalent to
  //
  // 10 |> (\x {x*x})()
)

fn square(a: int, b: int) -> int { a * b }

// `#` character can be used as a placeholder
// it binds the expression on the left side, which is only evaluated once
fn expensive() {
  print("expensive")
  2 * 2
}
tuple := expensive() |> (#, #, #) // prints "expensive" exactly once

// if the right-side is not a function with arity=1 or a call expression
// using one, it *must* include the `#` character to specify where the
// left-side should should be placed.

[1, 2, 3] |> sum // ok, function with arity=1
[1, 2, 3] |> sum() // ok, call to fn with arity=1

10 |> 5 + 5 // invalid, not call/fn, not using `#`
10 |> # + 5 // ok