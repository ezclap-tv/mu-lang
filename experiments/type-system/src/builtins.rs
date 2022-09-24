use std::fmt::Write;

use crate::ty;

pub fn emit(buf: &mut String) {
  write!(
    buf,
    "\
let _if = (v, a, b) => {{
  if (v) return a();
  else return b();
}};
let print = (v) => console.log(v);
let intToStr = (v) => v.toString();"
  )
  .unwrap();
}

pub fn bindings<'a>() -> ty::Bindings<'a> {
  let mut v = ty::Bindings::new();
  v.insert(
    "print".into(),
    ty::Type::Function(Box::new(ty::Function {
      param: ("v".into(), ty::Type::Builtin(ty::Builtin::Any)),
      ret: ty::Type::Builtin(ty::Builtin::Any),
    })),
  );
  v.insert(
    "intToStr".into(),
    ty::Type::Function(Box::new(ty::Function {
      param: ("n".into(), ty::Type::Builtin(ty::Builtin::Int)),
      ret: ty::Type::Builtin(ty::Builtin::Str),
    })),
  );
  v
}
