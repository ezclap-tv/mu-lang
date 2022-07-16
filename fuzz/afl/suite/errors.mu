type A { /*...*/ }
type B { /*...*/ }

fn fallible(v: bool) throws {
  if v: throw A
  else: throw B
}

fn fallible2() throws {
  // propagation
  v := try fallible()
  // or `try` as postfix
  v := fallible().try

  // unwrapping
  v := try! fallible()
  v := fallible().try!
}

v := fallible() // error: must handle failure
// match on errors
v := match fallible() {
  v => /*...*/,
  // specific error
  error A => /*...*/,
  // specific error with binding
  error A as e => /*...*/,
  // wildcard
  error _ => /*...*/
}