if a() { /*...*/ }
elif b() { /*...*/ }
else { /*...*/ }

do { /*...*/ }

// both of the above are expressions:
// the last expression in the body is the result
v0 := if v { "a" } else { "c" }
v1 := do { "value" }
// to ensure that nothing is returned, put a semicolon after the last expression
v2: null = do { "value"; }

// you can use control statements without braces in `if` statements
if true return
if true break
if true continue

for item in iterator { /*...*/ }

while condition() { /*...*/ }
loop { /*...*/ }

for i in 0..10 {
  print(i)
}

v := 10
while v >= 0 {
  print(v)
  v -= 1
}

v := 0
loop {
  if v == 5 break
  v += 1
}

// labelled loops (applies to `for` and `while`, too)
'a: loop {
  'b: loop {
    break 'a
  }
}
