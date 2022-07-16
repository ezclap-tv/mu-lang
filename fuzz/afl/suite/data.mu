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

class A[T /*: Bound*/] where T: Bound {
  // note: commas and semicolons are interchangeable and fully optional
  // the may be used to enhance readability:
  a: string, b: string, c: string

  type AssociatedType = T

  fn static_method() -> Self.AssociatedType { /*...*/ }
  // an instance method takes `self` as the first parameter
  fn instance_method(self) { /*...*/ }
}

a := A("a", "b", "c")
a := A(a: "a", b: "b", c: "c")

class Constructor {
  a, b, c: string
  rest: [string]

  fn new(parts: [string]) -> Self {
    Constructor(...parts[0..3], parts[3..])
  }
}

// classes are nominal types
class A { v: number }
type B = { v: number }
fn accepts_A(v: A) { /* ... */ }
fn accepts_B(v: B) { /* ... */ }
a := A(v: 0)
b := { v: 1 }
accepts_A(a) // ok
accepts_A(b) // error: parameter `v` only accepts instances of class `A`
accepts_B(a) // ok
accepts_B(b) // ok

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
