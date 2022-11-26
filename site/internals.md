# Internals

Currently, this is just a collection of notes about various implementation details and/or guidelines. This should eventually be polished up a bit into another `mdbook` (separate from the tour).

When adding new expressions, it is important to check if it should be added to `Token::can_begin_expr`, otherwise it may not be a valid `rhs` for an `expr_range`.

TODO: describe `expr_before_block`, it has an effect on `expr_class` and `expr_range`

