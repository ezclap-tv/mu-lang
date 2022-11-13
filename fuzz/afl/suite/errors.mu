class A {}
class B {}
class C {}

fn f() {
  throw A()
}

let v = try f() {
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