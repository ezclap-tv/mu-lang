fn perform_io(label: string) {
  task.sleep(1)
  print("{label} io done")
}

use foo;

pub fn exec() {
  print("first a");
  foo.perform_io("first");
  print("first b");
}

use foo;

pub fn exec() {
  print("second a");
  foo.perform_io("second");
  print("second b");
}


first.exec();
second.exec();


pub fn exec() {
  print("first a");
  spawn foo.perform_io("first");
  print("first b");
}

pub fn exec() {
  print("second a");
  spawn foo.perform_io("second");
  print("second b");
}


spawn f(expensive());

// spawn a block
// equivalent to `spawn (fn() { /*...*/ })()`
spawn {
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

let v = (spawn { return "test" }).join();
assert(v == "test");

let r = (spawn { throw "test" }).try_join();
if r.error {
  handle(r.error);
} else {
  use(r.value);
}