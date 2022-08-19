use std::alloc::{AllocError, Layout};
use std::pin::Pin;
use std::ptr::NonNull;
use std::rc::Rc;

use crate::dynamic_arena::DynamicArena;

pub type AstPtr<'arena, T> = Box<T, Arena<'arena>>;
pub type AstVec<'arena, T> = Vec<T, Arena<'arena>>;

#[derive(Clone)]
pub struct Arena<'arena> {
  arena: Pin<Rc<DynamicArena<'arena>>>,
}

impl<'arena> std::fmt::Debug for Arena<'arena> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let arena = self.arena.as_bumpalo();
    write!(f, "Arena {{ size: {}b }}", arena.allocated_bytes())
  }
}

impl<'arena> Arena<'arena> {
  pub fn new() -> Arena<'arena> {
    let arena = Pin::new(Rc::new(DynamicArena::new_bounded()));
    Self { arena }
  }

  pub fn boxed<T>(&self, value: T) -> AstPtr<'arena, T> {
    Box::new_in(value, self.clone())
  }

  pub fn vec<T>(&self) -> AstVec<'arena, T> {
    Vec::new_in(self.clone())
  }
  pub fn vec_with_capacity<T>(&self, cap: usize) -> AstVec<'arena, T> {
    let mut v = Vec::new_in(self.clone());
    v.reserve(cap);
    v
  }
}

unsafe impl<'arena> std::alloc::Allocator for Arena<'arena> {
  fn allocate(
    &self,
    layout: std::alloc::Layout,
  ) -> Result<std::ptr::NonNull<[u8]>, std::alloc::AllocError> {
    let bump = self.arena.as_bumpalo();
    bump.allocate(layout)
  }

  unsafe fn deallocate(&self, ptr: std::ptr::NonNull<u8>, layout: std::alloc::Layout) {
    let bump = self.arena.as_bumpalo();
    bump.deallocate(ptr, layout)
  }

  unsafe fn shrink(
    &self,
    ptr: NonNull<u8>,
    old_layout: Layout,
    new_layout: Layout,
  ) -> Result<NonNull<[u8]>, AllocError> {
    let bump = self.arena.as_bumpalo();
    bump.shrink(ptr, old_layout, new_layout)
  }

  unsafe fn grow(
    &self,
    ptr: NonNull<u8>,
    old_layout: Layout,
    new_layout: Layout,
  ) -> Result<NonNull<[u8]>, AllocError> {
    let bump = self.arena.as_bumpalo();
    bump.grow(ptr, old_layout, new_layout)
  }

  unsafe fn grow_zeroed(
    &self,
    ptr: NonNull<u8>,
    old_layout: Layout,
    new_layout: Layout,
  ) -> Result<NonNull<[u8]>, AllocError> {
    let bump = self.arena.as_bumpalo();
    bump.grow_zeroed(ptr, old_layout, new_layout)
  }
}

#[cfg(test)]
mod tests {
  use std::sync::atomic::{AtomicUsize, Ordering};
  use std::sync::Arc;

  use super::*;

  #[derive(Debug, Clone)]
  struct IncrementsOnDrop(Arc<AtomicUsize>);
  impl IncrementsOnDrop {
    pub fn new() -> Self {
      Self(Arc::new(AtomicUsize::new(0)))
    }

    pub fn get(&self) -> usize {
      self.0.load(Ordering::SeqCst)
    }
  }
  impl std::ops::Drop for IncrementsOnDrop {
    fn drop(&mut self) {
      self.0.fetch_add(1, Ordering::SeqCst);
    }
  }

  #[test]
  fn test_arena_box() {
    let value = IncrementsOnDrop::new();
    let arena = Arena::new();

    let box1 = arena.boxed(value.clone());
    assert_eq!(box1.get(), 0);
    std::mem::drop(box1);
    assert_eq!(value.get(), 1);
  }

  #[test]
  fn test_arena_vec() {
    let value = IncrementsOnDrop::new();
    let arena = Arena::new();
    let mut v = arena.vec();

    v.push(value.clone());
    v.push(value.clone());
    v.push(value.clone());
    std::mem::drop(arena);
    std::mem::drop(v);

    assert_eq!(value.get(), 3);
  }
}
