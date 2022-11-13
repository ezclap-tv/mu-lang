class Data { a: int; b: str; }

trait Default {
  fn default() -> Self;
}

trait Iterate {
  type Iter: Iterator;
  fn iter(self) -> Iter;
}

trait Iterator {
  type Item;
  fn next(self) -> Item?;
}

class Node[T] {
  value: T;
  next: Node[T]?;
}

class List[T] {
  head: Node[T]?;

  fn new() -> Self {
    List { head: null };
  }

  fn prepend(self, value: T) {
    let next = self.head;
    self.head = Node { value };
    self.head.next = next;
  }

  impl Iterate {
    type Iter = ListIter[T];
    fn iter(self) -> Iter {
      ListIter { node: self.head }
    }
  }
}

class ListIter[T] {
  node: Node[T];

  impl Iterator {
    type Item = Node[T];
    fn next(self) -> Item? {
      let node = self.node;
      self.node = node.next;
      node.value
    }
  }
}

let list = List.new();

for i in 0..10 {
  list.prepend(i);
}

for val in list {
  print(val);
}

trait Add[Rhs = Self] {
  type Output = Self;
  fn add(self, rhs: Rhs) -> Output;
}

trait Sub[Rhs = Self] {
  type Output = Self;
  fn sub(self, rhs: Rhs) -> Output;
}

class Complex {
  real: int;
  imag: int;

  impl Add {
    fn add(self, rhs: Self) -> Output {
      Complex {
        real: self.real + rhs.real,
        imag: self.imag + rhs.imag
      }
    }
  }

  impl Sub {
    fn sub(self, rhs: Self) -> Output {
      Complex {
        real: self.real - rhs.real,
        imag: self.imag - rhs.imag
      }
    }
  }
}
