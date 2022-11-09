#[macro_export]
macro_rules! bump_if_match {
  (
    ($self:ident, $expr:ident)
    $($p:pat => $body:expr),*
  ) => {
    loop {
      match $self.current().kind {
        $(
          $p => {
            $self.bump();
            $body
          }
        ),*
        _ => break Ok($expr),
      }
    }
  }
}
