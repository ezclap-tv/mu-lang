use std::collections::{HashMap, VecDeque};
use std::future::Future;
use std::marker::PhantomData;
use std::pin::Pin;
use std::rc::Rc;

use futures::FutureExt;

// pub type Queue<T> = concurrent_queue::ConcurrentQueue<T>;
pub type Queue<T> = VecDeque<T>;
pub type BoxFuture<T> = Pin<Box<dyn Future<Output = T> + Send>>;

pub type TaskError = Box<dyn std::error::Error + Send>;
pub type FnResult = Result<Object, Box<dyn std::error::Error + Send>>;
pub type AsyncFnResult = Box<dyn Future<Output = FnResult> + Send>;
pub type SyncFn = fn() -> FnResult;
pub type AsyncFn = fn() -> AsyncFnResult;
pub type TaskIoResult = Result<Object, TaskError>;

#[derive(Clone, Debug)]
pub enum Object {
  Str(String),
  Error(String),
  Function(Rc<Function>),
  NativeFnSync(SyncFn),
  NativeFnAsync(AsyncFn),
}

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
  GetGlobal(&'static str),
  Print,
  CallNative,
  Spawn,
}

#[derive(Debug)]
pub struct Function {
  pub name: String,
  // pub initial_stack: Vec<Object>,
  pub code: Vec<OpCode>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TaskId(usize);

pub struct Task<'a> {
  #[allow(dead_code)]
  id: TaskId,
  ip: usize,
  stack: Vec<Object>,
  function: Rc<Function>,
  // future: Option<tokio::task::JoinHandle<TaskIoResult>>,
  // future: Option<LocalBoxFuture<'a, TaskIoResult>>,
  future: Option<BoxFuture<TaskIoResult>>,
  pd: std::marker::PhantomData<&'a ()>,
}

impl<'a> Task<'a> {
  pub fn complete_io(&mut self, result: TaskIoResult) {
    match result {
      Ok(ok) => self.stack.push(ok),
      Err(e) => {
        self.stack.push(Object::Error(e.to_string()));
      }
    }
  }
}

impl<'a> std::future::Future for Task<'a> {
  type Output = Option<TaskIoResult>;

  fn poll(
    mut self: std::pin::Pin<&mut Self>,
    cx: &mut std::task::Context<'_>,
  ) -> std::task::Poll<Self::Output> {
    if let Some(future) = self.future.as_mut().map(|fut| fut.poll_unpin(cx)) {
      match future {
        std::task::Poll::Ready(r) => std::task::Poll::Ready(Some(r)),
        std::task::Poll::Pending => std::task::Poll::Pending,
      }
    } else {
      std::task::Poll::Ready(None)
    }
  }
}

impl<'a> std::fmt::Debug for Task<'a> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("Task")
      .field("id", &self.id)
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

pub struct Isolate<'a> {
  globals: HashMap<&'static str, Object>,
  tasks_so_far: usize,
  // Tasks that need to be polled when the isolate wakes up.
  pending_queue: Queue<Task<'a>>,
  // Tasks that have been finished and can be executed. This temporary queue is preserved between polls as an optimization.
  done_tasks: Queue<Task<'a>>,
  // Tasks that have been polled. This temporary queue is preserved between polls as an optimization.
  polled_queue: Queue<Task<'a>>,
}

impl<'a> std::future::Future for Isolate<'a> {
  type Output = ();

  fn poll(
    mut self: Pin<&mut Self>,
    cx: &mut std::task::Context<'_>,
  ) -> std::task::Poll<Self::Output> {
    assert!(self.done_tasks.is_empty());

    while let Some(mut task) = self.pending_queue.pop_front() {
      match Pin::new(&mut task).poll(cx) {
        std::task::Poll::Ready(result) => {
          if let Some(result) = result {
            task.complete_io(result);
          }
          self.done_tasks.push_back(task);
        }
        std::task::Poll::Pending => {
          self.polled_queue.push_back(task);
        }
      }
    }

    // TODO: handle errors. What do we do if a task errors out? Do we keep executing the other futures?

    while let Some(mut task) = self.done_tasks.pop_front() {
      match self.execute_task(&mut task) {
        TaskExecution::Yield => self.pending_queue.push_back(task),
        TaskExecution::Stop => (),
        TaskExecution::Continue => unreachable!(),
      }
    }

    // If we have new futures, we're immediately ready to wake up and poll them for the first time.
    // In all other cases we rely on each individual future to call wake() as soon as it is ready,
    // waking up the isolate as well.
    let has_new_futures = !self.pending_queue.is_empty();
    if has_new_futures {
      cx.waker().wake_by_ref();
    }

    // Move the tasks from the temporary polled queue back into the pending queue, so they can be re-polled
    // the next time the isolate wakes up
    let mut polled_queue = std::mem::replace(&mut self.polled_queue, Queue::new());
    self.pending_queue.append(&mut polled_queue);
    std::mem::swap(&mut self.polled_queue, &mut polled_queue);

    // The isolate is only done when it has no more tasks to execute
    if self.pending_queue.is_empty() {
      std::task::Poll::Ready(())
    } else {
      std::task::Poll::Pending
    }
  }
}

impl<'a> Isolate<'a> {
  pub fn new(globals: HashMap<&'static str, Object>) -> Self {
    Self {
      globals: globals,
      tasks_so_far: 0,
      pending_queue: VecDeque::with_capacity(1),
      polled_queue: VecDeque::with_capacity(1),
      done_tasks: VecDeque::with_capacity(1),
      // done_queue: Queue::unbounded(),
      // queue: Queue::unbounded(),
    }
  }

  pub async fn run(&mut self, f: Function) {
    self.queue_function(f);
    std::pin::Pin::new(self).await;
  }

  fn queue_function<F: Into<Rc<Function>>>(&mut self, f: F) {
    let id = self.next_task_id();
    let f = f.into();
    self.pending_queue.push_back(Task {
      id,
      ip: 0,
      stack: Vec::new(), //f.initial_stack.clone(),
      function: f,
      future: None,
      pd: PhantomData,
    });
  }

  fn execute_task(&mut self, task: &mut Task<'a>) -> TaskExecution {
    while task.ip < task.function.code.len() {
      let opcode = task.function.code[task.ip];
      task.ip += 1;

      match opcode {
        OpCode::GetGlobal(name) => {
          task.stack.push(
            self
              .globals
              .get(name)
              .expect(
                "Unknown global. TODO: this should be an isolate-isolate or a task-level panic.",
              )
              .clone(),
          );
        }
        OpCode::Spawn => {
          self.spawn(task.stack.pop().unwrap());
        }
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
      Object::Function(f) => println!("<fn {}>", f.name),
      Object::NativeFnSync(f) => println!("[fn @ {:p}]", f),
      Object::NativeFnAsync(f) => println!("[async fn @ {:p}]", f),
    }
  }

  fn spawn(&mut self, object: Object) {
    match object {
      Object::Function(f) => self.queue_function(f),
      Object::Str(_) => panic!("Can only queue mu functions, but got a String"),
      Object::Error(_) => panic!("Can only queue mu functions, but got an Error"),
      Object::NativeFnSync(_) => panic!("Can only queue mu functions, but got a native function"),
      Object::NativeFnAsync(_) => {
        panic!("Can only queue mu functions, but got a native async function")
      }
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
        task.future = Some(Pin::from(f()));
        return TaskExecution::Yield;
      }
      Object::Function(_) => unimplemented!(
        "Can't call functions since there's no call stack, you probably wanna spawn this one"
      ),
    }

    TaskExecution::Continue
  }

  fn next_task_id(&mut self) -> TaskId {
    self.tasks_so_far += 1;
    TaskId(self.tasks_so_far - 1)
  }
}

impl From<Function> for Object {
  fn from(f: Function) -> Self {
    Object::Function(Rc::new(f))
  }
}
