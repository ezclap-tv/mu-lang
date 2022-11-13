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
