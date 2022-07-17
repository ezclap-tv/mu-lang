use std::cell::Cell;
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

fn poll_n_test(n: usize) -> AsyncFnResult {
  println!("(debug) scheduling poll-n-times with n={n}");
  Box::new(poll_n::PollNFuture::new(n))
}

pub mod poll_n {
  use std::task::Poll;

  use super::*;

  pub struct PollNFuture {
    n: usize,
    so_far: Cell<usize>,
  }
  impl PollNFuture {
    pub fn new(n: usize) -> Self {
      Self {
        n,
        so_far: Cell::new(0),
      }
    }
  }
  impl std::future::Future for PollNFuture {
    type Output = FnResult;

    fn poll(
      self: std::pin::Pin<&mut Self>,
      cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
      println!(
        "(debug)     Polling poll-n-times with n={}, {} so far ",
        self.n,
        self.so_far.get()
      );
      if self.so_far.get() >= self.n {
        Poll::Ready(Ok(Object::Str(format!(
          "Finished after being polled {} times!",
          self.n
        ))))
      } else {
        self.so_far.set(self.so_far.get() + 1);
        cx.waker().clone().wake();
        Poll::Pending
      }
    }
  }
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

  //   vm.queue_function(Function {
  //     name: "Poll 3 Test".into(),
  //     initial_stack: vec![{
  //       fn poll_3_test() -> AsyncFnResult {
  //         poll_n_test(3)
  //       }

  //       Object::NativeFnAsync(poll_3_test)
  //     }],
  //     code: vec![OpCode::CallNative, OpCode::Print],
  //   });

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
