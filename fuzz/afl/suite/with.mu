type Disposable {
  fn dispose(Self)
}

class File where Self: Disposable {
  fid: int

  fn open(path: string) -> File { /* ... */ }
  fn close(self) { /* ... */ }

  fn dispose(self) { self.close() }
}

// only `Disposable`s may be used in `with`
with f := File.open("test.json") {
  use(f)
  // call to `f.dispose()` is inserted here
}

// `with` may contain multiple declarations
with
  a := fopen("a.json");
  b := fopen("b.json");
  c := fopen("c.json");
{

}