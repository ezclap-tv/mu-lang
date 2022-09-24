let _if = (v, a, b) => {
  if (v) return a();
  else return b();
};
let print = (v) => console.log(v);
let intToStr = (v) => v.toString();
let fib = (n) =>
  _if(
    n < 2,
    () => n,
    () => n * fib(n - 1)
  );
let v = fib(10);
print(v);

