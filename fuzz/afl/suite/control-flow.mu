f a() { /*...*/ }
else if b() { /*...*/ }
else { /*...*/ }

// both of the above are expressions:
// the last expression in the body is the result
let v0 = if v { "a" } else { "c" }

// you can use control statements without braces in `if` statements
if true return
if true throw
if true break
if true continue
if true for ...
if true while ...
if true loop ...

{
  let x = 0
} // x is not accessible after this point

// value of a `do` block is the last expression in the body
let v = do {
  let x = 0
  x
}

for item in iterator { /*...*/ }

while condition() { /*...*/ }
loop { /*...*/ }

for i in 0..10 {
  print(i)
}

let v = 10
while v >= 0 {
  print(v)
  v -= 1
}

let v = 0
loop {
  if v == 5 break
  v += 1
}