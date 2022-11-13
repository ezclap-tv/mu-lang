name = 1;
// compound assignments for all binary arithmetic and bitwise operators, and optional coalescing
// `ident OP= expr desugar to `ident = ident OP expr`
name += 1;
name -= 1;
name /= 1;
name *= 1;
name %= 1;
name **= 1;
// assigns to `name` only if `name` is null,
// and asserts that `name` is no longer null
name ??= 1;

2 + 2;
2 - 2;
2 / 2;
2 * 2;
2 % 2;
2 ** 2;
2 == 2;
2 != 2;
2 > 2;
2 >= 2;
2 < 2;
2 <= 2;
-2
!true;
true && true;
false || true;
a ?? b;

f(); // call
f?(); // optional call (won't call if `f` is null)
f.key; // field
f?.key; // optional field (won't access `key` if `f` is null)
f["key"]; // index
f?["key"]; // optional index (won't index if `f` is null)
