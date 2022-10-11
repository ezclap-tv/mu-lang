### Type System

- static: types are checked at compile-time
- nominal: types are related using identity
- strict: no null reference errors
- strong: all type conversions are explicit

### Language features

Literals (null, int, float, bool, string, f-string, tuple, array, record)

```rust
v := null;
v: null;

v: int;
v := 0; // int
v: float;
v := 0.0; // float

v := true; // bool
v: bool;

v := "test"; // string
v := "formatted {v}"; // string interpolation
v := "escaped curlies \{v}"; // exact value is `escaped curlies {v}`
// strings also support other escaped charaters such as `\n`, `\t`, `\x2800`, etc.
v := "\x2800";
v: string;

v := (v,); // tuple
v: (T,);
v := (v, v);
v: (T,T);

v := []; // array
v := [v, v, v, v];
v := [v; 1024];
v: [T];

v := { v: v, [v]: v, ...v }; // record
v: { v: T };
```

Variable declaration

```rust
// inferred type
name := 0;

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
-2
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

If, do

```rust
if a() { /*...*/ }
else if b() { /*...*/ }
else { /*...*/ }

do { /*...*/ }

// both of the above are expressions:
// the last expression in the body is the result
v0 := if v { "a" } else { "c" }
v1 := do { "value" }
// to ensure that nothing is returned, put a semicolon after the last expression
v2: null = do { "value"; }

// you can use control statements without braces in `if` statements
if true return
if true break
if true continue
if true for ...
if true while ...
if true loop ...
```

Loops (for, while, loop), continue/break

```rust
for item in iterator { /*...*/ }

while condition() { /*...*/ }
loop { /*...*/ }

for i in 0..10 {
  print(i)
}

v := 10
while v >= 0 {
  print(v)
  v -= 1
}

v := 0
loop {
  if v == 5 break
  v += 1
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
  if n < 2 { n }
  else n * fib(n - 1)
}

// return may be used to return early
fn test() {
  if something() { return }
  print("yo")
}

fn split(s: string, sep: string) -> [string] {
  out := [];
  start := 0;
  end := 0;

  for ch in s {
    if ch == sep {
      out.push(s[start..end])
      start = end;
    } else {
      end += 1;
    }
  }

  if current.len > 0 {
    out.push(current);
  }

  out
}

print(split("a,b,c", sep: ","))
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

class Node<T> {
  value: T;
  next: Node[T]?;
}

class List[T] {
  @head: Node[T]?;

  fn new() -> Self {
    List(head: null);
  }

  fn prepend(self, value: T) {
    next := self.head;
    self.head = Node(value);
    self.head.next = next;
  }

  impl Iterate {
    type Iter = ListIter[T];
    fn iter(self) -> Iter {
      ListIter(node: self.head)
    }
  }
}

class ListIter[T] {
  node: Node[T];

  impl Iterator {
    type Item = Node[T];
    fn next(self) -> Item? {
      node := self.node;
      self.node = node.next;
      node.value
    }
  }
}

list := List.new();

for i in 0..10 {
  list.prepend(i);
}

for val in list {
  print(val);
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

v := try f() catch {
  A => ...,
  B => ...,
  C => ...,
}

try {
  a();
  b();
  c();
} catch {
  A => ...
  B => ...
  C => ...
}
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

// you can export functions, classes, enums, and types
export fn a() {}
export class B {}
export enum C {}
export type D {}

// exports may also be grouped into a block export
fn e() {}
traits F
class G {}
// symbols exported this way may be renamed using `as`
export { e as g, F as E, G as F }

// module imports may be arbitrarily re-ordered by the runtime, which means that
// there is no guarantee of the order in which the "side effects" of imported modules are processed.
// the only guarantee is that code at the top-level in imported modules will be executed exactly once.
```

Concurrency

```rust
/*
Everything is asynchronous by default.
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
*/

// the built-in `task` module provides utilities related to managing tasks,
// such as `sleep`, `select`, `join`, etc.

// foo.mu
import task

fn perform_io(label: string) {
  // this is a native function which yields to the executor
  // note that timers are *not* precise. the value should be
  // treated as a minimum duration.
  task.sleep(1)
  print("{label} io done")
}

// first.mu
import foo

export fn exec() {
  print("first a");
  foo.perform_io("first");
  print("first b");
}

// second.mu
import foo

export fn exec() {
  print("second a");
  foo.perform_io("second");
  print("second b");
}

// main.mu
import first
import second

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
import foo

export fn exec() {
  print("first a");
  spawn foo.perform_io("first");
  print("first b");
}

// second.mu
import foo

export fn exec() {
  print("second a");
  spawn foo.perform_io("second");
  print("second b");
}

// main.mu
import first
import second

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
t := spawn f();

// which can be joined, causing the current task to wait for it to finish.
t.join();

// you can also check if a task is done before joining it:
if t.done {
  t.join();
}

// joining a task evaluates to the return value of the executed closure.
// in case of an exception, the exception is rethrown.
v := (spawn { return "test" }).join();
assert(v == "test");

// you may use the `try_join` function to return a result,
// which stores the exception instead of rethrowing it.
r := (spawn { throw "test" }).try_join();
if r.error {
  handle(r.error);
} else {
  use(r.value);
}
```
