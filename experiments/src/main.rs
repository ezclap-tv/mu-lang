use std::cell::Cell;
use std::collections::HashMap;
use std::time::Duration;

use experiments::future_scheduler::{AsyncFnResult, FnResult, Function, Isolate, Object, OpCode};

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
    let now = std::time::Instant::now();
    tokio::time::sleep(Duration::from_millis(millis)).await;
    Ok(Object::Str(format!(
      "Result of fn{id} after {}ms",
      now.elapsed().as_millis()
    )))
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
  let mut vm = Isolate::new({
    let mut globals = HashMap::new();

    globals.insert("hello_world", Object::NativeFnSync(hello_world));

    globals.insert("sleep_test_500", {
      fn sleep_test_500() -> AsyncFnResult {
        sleep_test(0, 5000)
      }
      Object::NativeFnAsync(sleep_test_500)
    });

    globals.insert("sleep_test_205", {
      fn sleep_test_205() -> AsyncFnResult {
        sleep_test(1, 205)
      }
      Object::NativeFnAsync(sleep_test_205)
    });

    globals.insert("sleep_test_200", {
      fn sleep_test_200() -> AsyncFnResult {
        sleep_test(2, 200)
      }
      Object::NativeFnAsync(sleep_test_200)
    });

    globals.insert("poll_3", {
      fn poll_3_test() -> AsyncFnResult {
        poll_n_test(3)
      }
      Object::NativeFnAsync(poll_3_test)
    });

    globals.insert(
      "async_subtask_1",
      Function {
        name: "async_subtask_1".into(),
        code: vec![
          OpCode::GetGlobal("sleep_test_500"),
          OpCode::CallNative,
          OpCode::Print,
        ],
      }
      .into(),
    );

    globals.insert(
      "async_subtask_2",
      Function {
        name: "async_subtask_2".into(),
        code: vec![
          OpCode::GetGlobal("sleep_test_205"),
          OpCode::CallNative,
          OpCode::Print,
        ],
      }
      .into(),
    );

    globals.insert(
      "async_subtask_3",
      Function {
        name: "async_subtask_3".into(),
        code: vec![
          OpCode::GetGlobal("sleep_test_200"),
          OpCode::CallNative,
          OpCode::Print,
          OpCode::GetGlobal("subtask_poll_3"),
          OpCode::Spawn,
        ],
      }
      .into(),
    );

    globals.insert(
      "subtask_poll_3",
      Function {
        name: "subtask_poll_3".into(),
        code: vec![
          OpCode::GetGlobal("poll_3"),
          OpCode::CallNative,
          OpCode::Print,
        ],
      }
      .into(),
    );

    globals
  });

  let root_task = Function {
    name: "Test".into(),
    code: vec![
      OpCode::GetGlobal("hello_world"),
      OpCode::CallNative,
      OpCode::Print,
      OpCode::GetGlobal("async_subtask_1"),
      OpCode::Spawn,
      OpCode::GetGlobal("async_subtask_2"),
      OpCode::Spawn,
      OpCode::GetGlobal("async_subtask_3"),
      OpCode::Spawn,
    ],
  };

  vm.run(root_task).await;
}
