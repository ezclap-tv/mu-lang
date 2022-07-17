use std::time::Duration;

use experiments::piggyback_scheduler::{AsyncFnResult, FnResult, Function, Object, OpCode, Vm};

fn hello_world() -> FnResult {
  Ok(Object::Str("Hello, World!".into()))
}

fn sleep_test(id: usize, millis: u64) -> AsyncFnResult {
  println!("(debug): Scheduling async fn{id}");
  let now = std::time::Instant::now();
  Box::new(async move {
    println!(
      "[async fn{id}] Function executed after {}μs! Now waiting for {millis}ms.",
      now.elapsed().as_micros()
    );
    tokio::time::sleep(Duration::from_millis(millis)).await;
    Ok(Object::Str(format!("Result of fn{id} after {millis}ms")))
  })
}

#[tokio::main]
async fn main() {
  //   let _runtime = tokio::runtime::Builder::new_multi_thread()
  //     .enable_all()
  //     .build()
  //     .unwrap().block_on(future);

  let mut vm = Vm::new();
  vm.queue_function(Function {
    name: "Test".into(),
    code: vec![OpCode::CallNative, OpCode::Print],
    initial_stack: vec![Object::NativeFnSync(hello_world)],
  });

  vm.queue_function(Function {
    name: "Async Test 1".into(),
    initial_stack: vec![{
      fn sleep_test_500() -> AsyncFnResult {
        sleep_test(0, 500)
      }

      Object::NativeFnAsync(sleep_test_500)
    }],
    code: vec![OpCode::CallNative, OpCode::Print],
  });

  vm.queue_function(Function {
    name: "Async Test 2".into(),
    initial_stack: vec![{
      fn sleep_test_205() -> AsyncFnResult {
        sleep_test(1, 205)
      }

      Object::NativeFnAsync(sleep_test_205)
    }],
    code: vec![OpCode::CallNative, OpCode::Print],
  });

  vm.queue_function(Function {
    name: "Async Test 3".into(),
    initial_stack: vec![{
      fn sleep_test_200() -> AsyncFnResult {
        sleep_test(2, 200)
      }

      Object::NativeFnAsync(sleep_test_200)
    }],
    code: vec![OpCode::CallNative, OpCode::Print],
  });

  vm.run();
}
