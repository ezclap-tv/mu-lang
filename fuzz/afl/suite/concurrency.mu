fn perform_io(label: string) {
  sleep(1)
  print("{label} io done")
}

// first.hr
print("first a")
perform_io("first")
print("first b")

// second.hr
print("second a")
perform_io("second")
print("second b")


// first.hr
print("first a")
spawn perform_io("first")
print("first b")

// second.hr
print("second a")
spawn perform_io("second")
print("second b")

spawn task()

// spawn a "block"
// equivalent to `spawn (fn() { /*...*/ })()`
spawn { /*...*/ }

// `spawn` returns a task handle
task := spawn { /*...*/ }
do_something()
if !task.done {
  task.join() // blocks until `task` finishes executing
}

// the `async.gather` function takes in multiple task handles, and blocks
// until all of them have finished executing.
async.gather([
  spawn { /*...*/ },
  spawn { /*...*/ },
])
