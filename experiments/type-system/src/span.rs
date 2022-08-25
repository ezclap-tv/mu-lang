// TODO: create `Spanned` type
// - the type should:
//   - auto-deref to the inner `T`
//   - transparently serialize/deserialize
//   - transparently display/debug fmt
//   - have a `Default` impl
// anything in the AST that needs a span (probably most things) will become type aliases for `Spanned<T>`.
// for example, `Expr` will be a type alias for `Spanned<ExprKind>`.
// parser should include a `.spanned` method which will wrap the Ok variant in `Spanned`
// the span should start on the previous token before parsing inner, and end on the previous token after parsing inner.
// this may require the parser to have its starting tokens (before the first bump) have spans of 0..0 instead of end..end
