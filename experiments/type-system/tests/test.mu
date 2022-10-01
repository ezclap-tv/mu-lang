let fib n: int -> int =
  if n < 2 then n
  else n * fib (n - 1)
;

print (readFile {path:"test.txt"});

let path = "v.txt";
let data = intToStr(fib (10));
print (path);
print (data);
writeFile {path:path, data:data};