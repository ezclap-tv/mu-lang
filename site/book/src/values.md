# Values

Mu divides values into primitive, compound, and user-defined types.

The primitives types are:

| type    | example |
| :------ | :------ |
| `null`  | `null`  |
| `int`   | `10`    |
| `float` | `3.14`  |
| `bool`  | `true`  |

The built-in compound types are:

| type     | example           |
| :------- | :---------------- |
| `tuple`  | `(a, b, c)`       |
| `array`  | `[a, b, c]`       |
| `range`  | `0..10`           |
| `slice`  | `[a, b, c][0..3]` |
| `string` | `"test"`          |

User-defined types are described in the chapter on [classes](./classes.md)

### Null

`null` exists as a built-in analogue to Rust's `Option` type, and is used in much the same way. Only nullable types may be `null`, which means that Mu does not re-introduce the [billion dollar mistake](https://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare/).

```rust,ignore
let v: int? = null;
v += 10; // error: cannot add-assign to `v`, as it is nullable
```

Some operators have nullable variants:

```rust,ignore
let foo: Foo? = Foo { n: 10 };
let v = foo?.n;
let v = foo?.bar?();
let v = foo?.map?()?["key"];
```

The operation will only be performed if the value to the left of the `?` is not `null`. If the value is `null`, it will simply propagate it:

```rust,ignore
let foo: Foo? = null;
let v: int? = foo?.n;
print(v); // prints `null`
```

The `??` operator yields the right value if the left value is null:

```rust,ignore
let v: int? = null;
print(v); // prints `null`
let v = v ?? 10;
print(v); // prints `10`
```

It is also short-circuiting, which means that it will only evaluate the right value if the left value is null:

```rust,ignore
let v: int? = 10;
print(v); // prints `10`
// expensive calculation is avoided because `v` is not `null`
let v = v ?? very_expensive_calculation();
print(v); // still prints `10`
```

### Integers and Floats

Numbers in Mu may be either integers or floats. The language supports some common arithmetic operators:

| operation          | example  | description                    |
| :----------------- | :------- | :----------------------------- |
| addition           | `a + b`  | adds `a` to `b`                |
| subtraction        | `a - b`  | subtracts `b` from `a`         |
| multiplication     | `a * b`  | multiplies `a` by `b`          |
| division           | `a / b`  | divides `a` by `b`             |
| modulo (remainder) | `a % b`  | divides `a` by `b`             |
| exponentiation     | `a ** b` | brings `a` to the power of `b` |

All arithmetic operators require that both values have the same type:

```rust,ignore
let x: int = 10;
let y: float = 3.14;

// error: type mismatch, expected `y` to have the same type as `x`
let z = x + y;

// explicit cast of `int` to `float`
let z = x.(float) + y; // ok
```

There are no bitwise operators, but all their functionality and some common compound operations are available through the built-in `bit` module:

```rust,ignore
let v = bit.shift_r(0b00110011, 2); // 0b00001100
let v = bit.toggle(v, 0); // 0b00001101
let v = bit.test(v, 0); // true
```

Integers may be written in binary (`0b`) or hexadecimal (`0x`) form, and may also contain underscores:

```rust,ignore
let dec = 100_000_000;
let bin = 0b0011_0011;
let hex = 0xDABBA_D00;
```

Floats may be written using [e-notation](https://en.wikipedia.org/wiki/Scientific_notation#E_notation):

```rust,ignore
let x = 1e-10;
```

The syntax of floats is slightly stricter than Rust's. A decimal point must always be followed by at least one digit:

```rust,ignore
let y = 1.; // invalid
let y = 1.0; // ok
```

### Booleans

Booleans represent truth values, and are either `true` or `false`. Mu supports common boolean operators:

<table>
  <thead>
    <tr><th>operation</th><th>example</th></tr>
  </thead>
  <tbody>
    <tr><td>negation</td><td><code>!v</code></td></tr>
    <tr><td>boolean and</td><td><code>v && v</code></td></tr>
    <tr><td>boolean or</td><td><code>v || v</code></td></tr>
  </tbody>
</table>

Mu also supports comparison operators which return boolean values:

| operation                   | example  |
| :-------------------------- | :------- |
| greater comparison          | `a > b`  |
| lesser comparison           | `a < b`  |
| greater or equal comparison | `a >= b` |
| lesser or equal comparison  | `a <= b` |
| equality                    | `a == b` |
| inequality                  | `a != b` |

### Tuples

Tuples are compound types which can hold any number of values, and each value may have a different type:

```rust,ignore
let a = 10;
let b = "test";
let c = true;

let t = (a, b, c);
```

Accessing tuple fields is done using numeric keys, and the first key is `0`:

```rust,ignore
let t = ("a", "b", "c");
print(t.0); // prints `a`
```

Tuples have a fixed length, which means that you can't append or prepend elements to a tuple after creating it:

```rust,ignore
let t = ("a", "b", "c");
t.3 = "d"; // error: tuple `t` only has 3 elements
```

### Ranges

A range represents a contiguous set of integers:

```rust,ignore
// equivalent to the set {0, 1, 2, 3, 4}
let r = 0..5;
```

Ranges may be inclusive, which means it will include the final integer:

```rust,ignore
// equivalent to the set {0, 1, 2, 3, 4, 5}
let r = 0..=5
```

Ranges may also be open on either side. Being open on the left side implies a starting index of `0`:

```rust,ignore
let r = ..5; // same as 0..5
```

And being open on the right side implies an end index of infinity.

```rust,ignore
let r = 5..; // same as 5..∞
// Note: `∞` is not a real syntax in Mu
```

Here is the full syntax table for ranges:

| type                 | syntax                                                          |
| :------------------- | :-------------------------------------------------------------- |
| exclusive full       | `..`                                                            |
| exclusive left-open  | `..n`                                                           |
| exclusive right-open | `n..`                                                           |
| exclusive closed     | `n..m`                                                          |
| inclusive full       | (`..=`) - syntax error, inclusive ranges may not be right-open  |
| inclusive left-open  | `..=n`                                                          |
| inclusive right-open | (`n..=`) - syntax error, inclusive ranges may not be right-open |
| inclusive closed     | `n..=m`                                                         |

### Arrays, and Slices

An array is a list of values which all have the same type:

```rust,ignore
let a = ["a", "b", "c"];
```

Values in an array may be access using the index operator. Arrays in Mu use 0-based indexing, which means the first index is `0`:

```rust,ignore
let a = ["a", "b", "c"];
print(a[0], a[1], a[2]); // prints `a b c`
```

Arrays do not have a fixed length, which means you can add and remove elements as you see fit:

```rust,ignore
let a = [];
a.push("a");
a.push("b");
a.push("c");
print(a.pop()); // prints `c`
print(a.len()); // prints `2`
```

A range may be used to slice arrays. A slice allows you to access a contiguous section of an array without copying the array:

```rust,ignore
let a = ["a", "b", "c", "d", "e"];
let s: Slice<string> = a[1..4];
print(s); // prints `["b", "c", "d"]`
```

The slice does not store any of the elements it has access to, those are still stored in the original array. A slice may be created from another slice, in which case both will refer to the same underlying array:

```rust,ignore
let a = ["a", "b", "c", "d", "e"];
let s = a[1..4];
print(s[0..2]); // prints `["b", "c"]`
```

Any number of slices may refer to the same array. Any mutations to an array also affect any slices that refer to it:

```rust,ignore
let mut a = ["a", "b", "c", "d", "e"];

let s = a[1..4];
print(s); // prints `["b", "c", "d"]`

a[2] = "f";
print(s); // prints `["b", "f", "d"]`
```

If you attempt to index an array or a slice element with an index lower than 0, or higher than the length of the array, it will throw an out of bounds exception:

```rust,ignore
let a = [];
print(a[1]); // throws an out of bounds exception
```

Attempting to slice an array without enough elements for the entire slice will fail in the same way:

```rust,ignore
let a = [];
print(a[0..10]); // throws an out of bounds exception
```

Slices have the type `Slice<T>`, where `T` is the element type. Their functionality is almost the same as that of arrays, the major difference being that you may not do anything to a slice that would change its length. `push`, `pop`, and `remove` are examples of methods which are not available for slices.

### Strings

Mu strings are represented as an immutable sequence of utf8-encoded characters, just like Rust's `str` type. They are written inside of double quotes:

```rust,ignore
let s = "Hello, world!";
```

Strings support escapes:

|            | result                   |
| :--------- | :----------------------- |
| `\n`       | newline                  |
| `\r`       | carriage return          |
| `\t`       | tab                      |
| `\\`       | backslash                |
| `\"`       | double quote             |
| `\u{7FFF}` | unicode (up to 4 digits) |

Strings can be concatenated using `+`:

```rust,ignore
let a = "Hello, ";
let b = "world!";
print(a + b); // prints `Hello, world!`
```

Each concatenation of two strings has to allocate a new string. You can use the built-in `str.builder` function to build up larger strings:

```rust,ignore
let mut s = str.builder();
s.push("lorem ipsum");
s.push("dolor sit amet");
let s = s.build();
```

Strings can also be sliced, just like arrays:

```rust,ignore
let s = "Hello, world!";
print(s[..]);
```

There is no need for a distinction between a string slice and a string (both are immutable and have fixed lengths), so a sliced `string` is still just a `string`.

