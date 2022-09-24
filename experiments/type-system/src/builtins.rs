use std::fmt::Write;

use crate::ty;

pub fn emit(buf: &mut String) {
  write!(
    buf,
    r#"
let _if = (v, a, b) => {{
  if (v) return a();
  else return b();
}};
let print = (v) => console.log(v);
let intToStr = (v) => v.toString();
let _fs = require("fs");
let readFile = ({{ path }}) => _fs.readFileSync(path, "utf-8");
let writeFile = ({{ path, data }}) => _fs.writeFileSync(path, data, "utf-8");
"#
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
  v.insert(
    "readFile".into(),
    ty::Type::Function(Box::new(ty::Function {
      param: (
        "file".into(),
        ty::Type::Record(ty::Record {
          fields: vec![ty::Field {
            ident: "path".into(),
            ty: ty::Type::Builtin(ty::Builtin::Str),
          }],
        }),
      ),
      ret: ty::Type::Builtin(ty::Builtin::Str),
    })),
  );
  v.insert(
    "writeFile".into(),
    ty::Type::Function(Box::new(ty::Function {
      param: (
        "file".into(),
        ty::Type::Record(ty::Record {
          fields: vec![
            ty::Field {
              ident: "data".into(),
              ty: ty::Type::Builtin(ty::Builtin::Str),
            },
            ty::Field {
              ident: "path".into(),
              ty: ty::Type::Builtin(ty::Builtin::Str),
            },
          ],
        }),
      ),
      ret: ty::Type::Builtin(ty::Builtin::Str),
    })),
  );
  v
}
