fn name(a: A, b: B, c: C) {/*...*/}
fn name[T](a: A, b: B, c: C) -> T {/*...*/}
fn name[T, E](a: A, b: B, c: C) -> T where T: Bound {/*...*/}

fn name[T, E](a: A, b: B, c: C) -> T
  where
    A: IA,
    B: IB,
    C: IC
{
}

let square = \x {x*x}

fn fib(n: int): int {
  if n < 2 { n }
  else n * fib(n - 1)
}

fn test() {
  if something() { return }
  print("yo")
}


fn foo(n) {
  n * 2
}
foo(10)
