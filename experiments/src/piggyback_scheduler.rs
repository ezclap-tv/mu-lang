use std::marker::PhantomData;

use futures::Future;
use tokio::sync::mpsc::error::TryRecvError;
use tokio::sync::mpsc::{channel, Receiver};

pub type Queue<T> = concurrent_queue::ConcurrentQueue<T>;

pub type FnResult = Result<Object, Box<dyn std::error::Error + Send>>;
pub type AsyncFnResult = Box<dyn Future<Output = FnResult> + Send>;
pub type SyncFn = fn() -> FnResult;
pub type AsyncFn = fn() -> AsyncFnResult;

#[derive(Clone, Debug)]
pub enum Object {
  Str(String),
  Error(String),
  NativeFnSync(SyncFn),
  NativeFnAsync(AsyncFn),
}
pub type TaskIoResult = Result<Object, TaskError>;

#[derive(Debug)]
pub enum TaskError {
  UnexpectedFailure,
  Error(Box<dyn std::error::Error + Send>),
}

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
  Print,
  CallNative,
}

pub struct Function {
  pub name: String,
  pub initial_stack: Vec<Object>,
  pub code: Vec<OpCode>,
}

pub struct Task<'a> {
  ip: usize,
  stack: Vec<Object>,
  function: Function,
  future: Option<Receiver<TaskIoResult>>, // future: Option<LocalBoxFuture<'a, TaskIoResult>>,
  pd: std::marker::PhantomData<&'a ()>,
}

impl<'a> Task<'a> {
  pub fn complete_io(&mut self, result: TaskIoResult) {
    match result {
      Ok(ok) => self.stack.push(ok),
      Err(e) => match e {
        TaskError::UnexpectedFailure => self
          .stack
          .push(Object::Error("Async function failed unexpectedly".into())),
        TaskError::Error(e) => self.stack.push(Object::Error(e.to_string())),
      },
    }
  }

  pub fn poll(&mut self) -> std::task::Poll<Option<TaskIoResult>> {
    if let Some(f) = self.future.as_mut() {
      match f.try_recv() {
        Err(TryRecvError::Empty) => std::task::Poll::Pending,
        Ok(res) => {
          self.future = None;
          std::task::Poll::Ready(Some(res))
        }
        Err(TryRecvError::Disconnected) => {
          self.future = None;
          std::task::Poll::Ready(Some(Err(TaskError::UnexpectedFailure)))
        }
      }
    } else {
      std::task::Poll::Ready(None)
    }
  }
}

// impl<'a> future::Future for Task<'a> {
//   type Output = Option<TaskIoResult>;

//   fn poll(
//     self: std::pin::Pin<&mut Self>,
//     cx: &mut std::task::Context<'_>,
//   ) -> std::task::Poll<Self::Output> {
//     if let Some(future) = self.future {
//       match future.poll_unpin(cx) {
//         std::task::Poll::Ready(_) => todo!(),
//         std::task::Poll::Pending => todo!(),
//       }
//     } else {
//       std::task::Poll::Ready(None)
//     }
//   }
// }

impl<'a> std::fmt::Debug for Task<'a> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("Task")
      .field("ip", &self.ip)
      .field("stack", &self.stack)
      .field("function", &self.function.name)
      .finish()
  }
}

pub enum TaskExecution {
  Stop,
  Yield,
  Continue,
}

pub struct Vm<'a> {
  queue: Queue<Task<'a>>,
}

impl<'a> Vm<'a> {
  pub fn new() -> Self {
    Self {
      queue: Queue::unbounded(),
    }
  }

  pub fn queue_function(&mut self, f: Function) {
    self
      .queue
      .push(Task {
        ip: 0,
        stack: f.initial_stack.clone(),
        function: f,
        future: None,
        pd: PhantomData,
      })
      .unwrap();
  }

  pub fn run(&self) {
    while let Ok(mut task) = self.queue.pop() {
      if let std::task::Poll::Ready(result) = task.poll() {
        if let Some(result) = result {
          task.complete_io(result);
        }

        match self.execute_task(&mut task) {
          TaskExecution::Stop => continue,
          TaskExecution::Yield => self.queue.push(task).unwrap(),
          TaskExecution::Continue => unreachable!(),
        }
      } else {
        self.queue.push(task).unwrap();
      }
    }
  }

  pub fn execute_task(&self, task: &mut Task<'a>) -> TaskExecution {
    while task.ip < task.function.code.len() {
      let opcode = task.function.code[task.ip];
      task.ip += 1;

      match opcode {
        OpCode::Print => {
          self.print(&task.stack.pop().unwrap());
        }
        OpCode::CallNative => {
          if let TaskExecution::Yield = self.call(task) {
            return TaskExecution::Yield;
          }
        }
      }
    }

    TaskExecution::Stop
  }

  fn print(&self, object: &Object) {
    match object {
      Object::Str(s) => println!("{}", s),
      Object::Error(s) => println!("Error({:?})", s),
      Object::NativeFnSync(f) => println!("[fn @ {:p}]", f),
      Object::NativeFnAsync(f) => println!("[async fn @ {:p}]", f),
    }
  }

  fn call(&self, task: &mut Task<'_>) -> TaskExecution {
    let object = task.stack.pop().unwrap();
    match object {
      Object::Str(_) => panic!("String is not a callable object"),
      Object::Error(_) => panic!("Error is not a callable object"),
      Object::NativeFnSync(f) => task.stack.push(match f() {
        Ok(res) => res,
        Err(e) => Object::Error(e.to_string()),
      }),
      Object::NativeFnAsync(f) => {
        let (tx, rx) = channel(1);
        task.future = Some(rx);
        let future: std::pin::Pin<_> = f().into();
        tokio::spawn(async move {
          tx.try_send(match future.await {
            Ok(ok) => TaskIoResult::Ok(ok),
            Err(e) => TaskIoResult::Err(TaskError::Error(e)),
          })
          .unwrap();
        });
        return TaskExecution::Yield;
      }
    }

    TaskExecution::Continue
  }
}
