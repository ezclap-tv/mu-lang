let fib n: int -> int =
  if n < 2 then n
  else n * fib (n - 1)
;

let v = fib (10);
print (v);