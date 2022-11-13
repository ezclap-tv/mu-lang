### Type System

- static: types are checked at compile-time
- nominal: types are related using identity
- strict: no null reference errors
- strong: all type conversions are explicit

### Language features

Literals (null, int, float, bool, string, tuple, array)

```rust
// v : null;
let v = null;

// v : int;
let v = 0;
// v : float;
let v = 0.0;

// v : bool;
let v = true;

// v : string
let v = "test";

// v : (T,)
let v = (v,);
// v : (T, T)
let v = (v, v);

// v : [T]
let v = [];
let v = [v, v, v, v];
let v = [v; 1024];
let v = v[0..512];
```

Variable declaration

```rust
// inferred type
let name = 0;

// explicit type
// this declaration shadows the previous one, so its type may differ
name: string = "value";
```

Operators

- Arithmetic (`+`, `-`, `/`, `*`, `**`)
- Equality (`==`, `!=`)
- Comparison (`>`, `>=`, `<`, `<=`)
- Boolean (`not`/`!`, `and`/`&&`, `or`/`||`)

```rust
2 + 2;
2 - 2;
2 / 2;
2 * 2;
2 % 2;
2 ** 2;
2 == 2;
2 != 2;
2 > 2;
2 >= 2;
2 < 2;
2 <= 2;
-2;
!true;
true && true;
false || true;
a ?? b;
```

Assignment and compound assignments

```rust
name = 1;
// compound assignments for all binary arithmetic and bitwise operators, and optional coalescing
// `ident OP= expr desugar to `ident = ident OP expr`
name += 1;
name -= 1;
name /= 1;
name *= 1;
name %= 1;
name **= 1;
// assigns to `name` only if `name` is null,
// and asserts that `name` is no longer null
name ??= 1;
```

Postfix operators (call, field, index, cast)

```rust
f(); // call
f?(); // optional call (won't call if `f` is null)
f.key; // field
f?.key; // optional field (won't access `key` if `f` is null)
f["key"]; // index
f?["key"]; // optional index (won't index if `f` is null)


class Data { a: int; b: str; }

// type cast
let v = Json.parse("{\"a\":0, \"b\":\"test\"}").(Data);
```

Flow control, blocks, do

```rust
if a() { /*...*/ }
else if b() { /*...*/ }
else { /*...*/ }

{
  let x = 0;
} // x is not accessible after this point

// the last expression in the body is the result
let v0 = if v { "a" } else { "c" };
// same for `do`:
let v = do { 0 };
// and `try`, which is introduced later:
let v = try { a() } catch (_) { b };
// `b` is returned if an exception is thrown from the call to `a`
```

Loops (for, while, loop), continue/break

```rust
for item in iterator { /*...*/ }
while condition() { /*...*/ }
loop { /*...*/ }

for i in 0..10 {
  print(i);
}

let v = 10;
while v >= 0 {
  print(v);
  v -= 1;
}

let v = 0;
loop {
  if v == 5 { break }
  v += 1;
}
```

Functions (generics, variadics, return)

```rust
fn name(a: A, b: B, c: C) {/*...*/}
fn name[T](a: A, b: B, c: C) -> T {/*...*/}
fn name[T, E](a: A, b: B, c: C) -> T where T: Bound {/*...*/}

fn name[T, E](a: A, b: B, c: C) -> T
  where
    A: IA,
    B: IB,
    C: IC
{
  /*...*/
}

// anonymous function
let square = \x {x*x};

fn fib(n: int): int {
  if n < 2 { n }
  else { n * fib(n - 1) }
}

// return may be used to return early
fn test() {
  if something() { return }
  print("yo");
}

// functions may also be written using dynamic typing
// equivalent to:
// fn foo(f: any, n: any) -> any { f(n) }
fn apply(f, n) { f(n) }

foo(square, 10);
```

Classes, traits

```rust
trait Default {
  fn default() -> Self;
}

trait Iterate {
  type Iter: Iterator;
  fn iter(self) -> Iter;
}

trait Iterator {
  type Item;
  fn next(self) -> Item?;
}

class Node[T] {
  value: T;
  next: Node[T]?;
}

class List[T] {
  head: Node[T]?;

  fn new() -> Self {
    List { head: null };
  }

  fn prepend(self, value: T) {
    let next = self.head;
    self.head = Node { value };
    self.head.next = next;
  }

  impl Iterate {
    type Iter = ListIter[T];
    fn iter(self) -> Iter {
      ListIter { node: self.head }
    }
  }
}

class ListIter[T] {
  node: Node[T];

  impl Iterator {
    type Item = Node[T];
    fn next(self) -> Item? {
      let node = self.node;
      self.node = node.next;
      node.value
    }
  }
}

let list = List.new();

for i in 0..10 {
  list.prepend(i);
}

for val in list {
  print(val);
}

trait Add[Rhs = Self] {
  type Output = Self;
  fn add(self, rhs: Rhs) -> Output;
}

trait Sub[Rhs = Self] {
  type Output = Self;
  fn sub(self, rhs: Rhs) -> Output;
}

class Complex {
  real: int;
  imag: int;

  impl Add {
    fn add(self, rhs: Self) -> Output {
      Complex {
        real: self.real + rhs.real,
        imag: self.imag + rhs.imag
      }
    }
  }

  impl Sub {
    fn sub(self, rhs: Self) -> Output {
      Complex {
        real: self.real - rhs.real,
        imag: self.imag - rhs.imag
      }
    }
  }
}
```

Exceptions

```rust
class A {}
class B {}
class C {}

fn f() {
  throw A()
}

let v = try f() catch (e) {
  /* ... */
}

try {
  a();
  b();
  c();
} catch (e) {
  /* ... */
}
```

Modules

```rust
use module;
use module.A;
use module.{A, B};

use nested.module.A;
use nested.module.{A, B};

use {
  a.{A, B},
  b.{C, D},
  nested.{
    d.{E, F},
    f.{G, H},
  }
};

// you can export functions, classes, traits, and type aliases
pub fn a() {}
pub class B {}
pub trait C {}
pub type D = B;

// module imports may be arbitrarily re-ordered by the runtime, which means that
// there is no guarantee of the order in which the "side effects" of imported modules are processed.
// the only guarantee is that code at the top-level in imported modules will be executed exactly once.
```

Concurrency

```rust
/*
All execution happens within the context of a task.
Whenever you do any I/O, the task is blocked until that I/O finishes,
but other tasks may be scheduled to run during during that time.

you may also know tasks under names such as:
- stackful coroutines
- goroutines
- fibers
- green threads

The runtime is single-threaded, which means that no synchronization
is ever needed, at the expense of not being able to parallelize computation.

Note that despite being single-threaded, the runtime may interrupt any task
at any point, and begin executing other scheduled tasks. This is known as
preemptive multitasking, as opposed to cooperative multitasking, where these
interrupts may only happen at well-defined points in the program. The reason
for preemptive multitasking is to allow the runtime to implement CPU time
and other resource limits.
*/

// the built-in `task` module provides utilities related to managing tasks,
// such as `sleep`, `select`, `join`, etc.

// foo.mu
use task;

fn perform_io(label: string) {
  // this is a native function which yields to the executor
  // note that timers are *not* precise. the value should be
  // treated as a minimum duration.
  task.sleep(1)
  print("{label} io done")
}

// first.mu
use foo;

pub fn exec() {
  print("first a");
  foo.perform_io("first");
  print("first b");
}

// second.mu
use foo;

pub fn exec() {
  print("second a");
  foo.perform_io("second");
  print("second b");
}

// main.mu
use first;
use second;

first.exec();
second.exec();

/*
assuming that the I/O in `first.mu` completes before `second.mu`,
running `main.mu` would produce the following logs:

first a
second a
first io done
first b
second io done
second b

you never know when the runtime will suspend your module due to I/O,
but you also don't have to *care* about when the runtime will suspend
your code! it's easier to reason about, as all code simply looks
fully synchronous. but there are scenarios in which you explicitly
don't want to suspend execution until some I/O finishes, or you want
to perform more than one instance of I/O at the same time.
for these use cases, you can use the `spawn` keyword.
*/

// first.mu
use foo;

pub fn exec() {
  print("first a");
  spawn foo.perform_io("first");
  print("first b");
}

// second.mu

pub fn exec() {
  print("second a");
  spawn foo.perform_io("second");
  print("second b");
}

// main.mu
use first;
use second;

first.exec();
second.exec();

/*
executing `main.mu` will now print:

first a
first b
second a
second b
first io done
second io done

those last two prints may switch places if `second` completes first.
this is because the language does not guarantee the order of
execution of spawned tasks. if you need to guarantee the order
of execution, don't use `spawn`.
*/

/*
spawning a function call

this eagerly evaluates arguments,
but the actual execution of `f` does not begin immediately.
when exactly execution begins is unspecified.
*/
spawn f(expensive());

// spawn a block
// equivalent to `spawn (fn() { /*...*/ })()`
spawn {
  // because the block is a closure, `return` ends the task.
  // the return value becomes the result of the task
  return "baz"
}

// `spawn` returns a task handle
let t = spawn f();

// which can be joined, causing the current task to wait for it to finish.
t.join();

// you can also check if a task is done before joining it:
if t.done {
  t.join();
}

// joining a task evaluates to the return value of the executed closure.
// in case of an exception, the exception is rethrown.
let v = (spawn { return "test" }).join();
assert(v == "test");

// you may use the `try_join` function to return a result,
// which stores the exception instead of rethrowing it.
let r = (spawn { throw "test" }).try_join();
if r.error {
  handle(r.error);
} else {
  use(r.value);
}
```
