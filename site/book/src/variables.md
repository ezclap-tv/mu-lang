# Variables

Mu supports two kinds of variables:

Immutable:

```rust,ignore
let v = 10;
print(v);
```

And mutable:

```rust,ignore
let mut v = 10;
```

### Mutability

Mutation means change. Mutability refers to a variable's **ability** to be **mut**ated.

There are many ways to mutate a variable:

```rust,ignore
let mut v = 10;
print(v); // prints `10`
v = 20; // simple assignment
print(v); // prints `20`
v += 1; // compound assignment
print(v); // prints `21`
```

Compound assignment above refers to the combination of a binary expression, and an assignment. Here is the list of supported compound assignment operators:

| operator           | example   | desugaring   | description                          |
| :----------------- | :-------- | :----------- | :----------------------------------- |
| addition           | `v += 1`  | `v = v + 1`  | adds `1` to `v`                      |
| subtraction        | `v -= 1`  | `v = v - 1`  | subtracts `1` from `v`               |
| division           | `v /= 1`  | `v = v / 1`  | divides `v` by `1`                   |
| multiplication     | `v *= 1`  | `v = v * 1`  | multiplies `v` by `1`                |
| modulo (remainder) | `v %= 1`  | `v = v % 1`  | sets `v` to the remainder of `v / 1` |
| exponentation      | `v **= 1` | `v = v ** 1` | raises `v` to the power of `1`       |
| null coalescing    | `v ??= 1` | `v = v ?? 1` | sets `v` to `1` if `v` is null       |

If a variable is immutable, it may not change:

```rust,ignore
let v = 10;
print(v); // prints `10`
v = 20; // error: cannot assign to `v`, as it is not mutable
```

Immutability in Mu is deep, which means it affects not only the variable, but also any fields the value may have:

```rust,ignore
let v = Foo { value: 10 };
v.value = 20; // error: cannot assign to `v.value`, as it is not mutable
```

It is not possible to assign an immutable value to a mutable variable or field:

```rust,ignore
let foo = Foo { value: 10 };
let mut bar = foo; // error: value cannot be assigned to `bar`, as it is not mutable
let mut bar = Bar { foo: foo }; // error: value cannot be assigned to `foo`, as it is not mutable
```

This ensures that immutable values are _truly_ immutable.

We'll expand on this in the chapter about [functions](./functions.md), and once more in [classes](./classes.md)

### Type inference

By default, a variable's type is inferred from the value that's being assigned to it:

```rust,ignore
let v = 10;
```

You may instead choose to annotate the variable with a type:

```rust,ignore
let v: int = 10;
```

In which case, the assignment will fail if the value's type does not match the annotation:

```rust,ignore
// error: mismatched type - expected `int`, found `string`.
let v: int = "i'm not an int";
```

### Scope

Variables are block-scoped, which means that they may not be accessed outside of the block they were declared in:

```rust
{
  let v = 10;
  print(v); // prints `10`
  {
    print(v); // prints `10`
  }
}
print(v); // error: `v` is not declared in this scope
```

### Shadowing

Variables may be shadowed, which means that it is possible to re-declare them:

```rust,ignore
let v = 10;
print(v); // prints `10`
let v = 20;
print(v); // prints `20`
```

The new declaration may have a different type:

```rust,ignore
let v = 10;
print(v); // prints `10`
let v = "Hello, world!";
print(v); // prints `Hello, world!`
```

Variables may be shadowed in inner scopes as well:

```rust,ignore
let v = 10;
{
  let v = 20;
  print(v); // prints `20`
}
print(v); // prints `10`
```

&nbsp;

