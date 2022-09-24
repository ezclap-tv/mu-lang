## let (var)

```rust
let <ident:name> = <expr:value>
```

```js
let <raw(name)> = <emit(value)>;
```

Example

```rust
let x = 5
```

```js
let x = 5;
```

## let (var) in

```rust
let <ident:name> = <expr:body> in <expr:scope>
```

```js
(()=>{
  let <raw(name)> = <emit(body)>;
  return <emit(scope)>;
})()
```

Example

```rust
let x = 5 in print (x * x)
```

```js
(() => {
  let x = 5;
  return _print(x * x);
})();
```

## let (func)

```rust
let <ident:name> <ident:arg>: <_arg_type> -> <_ret_type> = <expr:body>
```

```js
let <emit(name)> = (<emit(arg)>) => <emit(body)>;
```

Example

```rust
let square n: int -> int = n * n
```

```js
let square = (n) => n * n;
```

## let (func) in

```rust
let <ident:name> <ident:arg>: <_arg_type> -> <_ret_type> = <expr:body>
in <expr:scope>
```

```js
(()=>{
  let <emit(name)> = (<emit(arg)>) => <emit(body)>;
  return <emit(scope)>;
})()
```

Example

```rust
let square n: int -> int = n * n
in print (square (10))
```

```js
(() => {
  let square = (n) => n * n;
  return _print(square(10));
})();
```

## if

Uses `_if` helper to maintain expression semantics, defined (in JS) as:

```js
function _if(cond, a, b) {
  if (cond) return a();
  else return b();
}
```

```rust
if <expr:cond> then <expr:then> else <expr:else>
```

```js
_if(
  <emit(cond)>,
  () => <emit(then)>,
  () => <emit(else)>,
)
```

Example

```rust
if true then print ("true") else print ("false")
```

```js
_if(
  true,
  () => _print("true"),
  () => _print("false")
);
```

## simple expr

Emit verbatim

## prefix expr

Transform `~` to `-`

```rust
~10
```

```js
-10;
```

## postfix expr

Transform record call to regular call with record argument

```rust
f {v:10}
```

```js
f({ v: 10 });
```

