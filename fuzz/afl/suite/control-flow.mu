if a() { /*...*/ }
else if b() { /*...*/ }
else { /*...*/ }

{
  let x = 0;
} // x is not accessible after this point

// the last expression in the body is the result
let v0 = if v { "a" } else { "c" };
// same for `do`:
let v = do { 0 };
// and `try`, which is introduced later:
let v = try { a() } catch (_) { b };
// `b` is returned if an exception is thrown from the call to `a`

for item in iterator { /*...*/ }
while condition() { /*...*/ }
loop { /*...*/ }

for i in 0..10 {
  print(i);
}

let v = 10;
while v >= 0 {
  print(v);
  v -= 1;
}

let v = 0;
loop {
  if v == 5 { break }
  v += 1;
}
