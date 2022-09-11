use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::iter::FromIterator;
use std::str::FromStr;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

// TODO: spans

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Type<'a> {
  Builtin(Builtin),
  Record(Record<'a>),
  Function(Box<Function<'a>>),
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum Builtin {
  Unit,
  Int,
  Bool,
  Str,
  // TODO: remove this after implementing polymorphism
  // currently it's only used to describe the type of `print`
  Any,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Record<'a> {
  pub fields: Vec<Field<'a>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Field<'a> {
  pub ident: Cow<'a, str>,
  pub ty: Type<'a>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Function<'a> {
  pub param: (Cow<'a, str>, Type<'a>),
  pub ret: Type<'a>,
}

impl<'a> fmt::Display for Type<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Type::Builtin(v) => write!(f, "{v}"),
      Type::Record(v) => {
        write!(f, "{{")?;
        let mut fields = v.fields.iter().peekable();
        while let Some(field) = fields.next() {
          write!(f, "{}:{}", field.ident, field.ty)?;
          if fields.peek().is_some() {
            write!(f, ", ")?;
          }
        }
        write!(f, "}}")?;
        Ok(())
      }
      Type::Function(v) => write!(f, "({}:{}) -> {}", v.param.0, v.param.1, v.ret),
    }
  }
}

impl fmt::Display for Builtin {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Builtin::Unit => write!(f, "unit"),
      Builtin::Int => write!(f, "int"),
      Builtin::Bool => write!(f, "bool"),
      Builtin::Str => write!(f, "str"),
      Builtin::Any => write!(f, "any"),
    }
  }
}

impl FromStr for Builtin {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "int" => Ok(Builtin::Int),
      "bool" => Ok(Builtin::Bool),
      "str" => Ok(Builtin::Str),
      _ => Err(()),
    }
  }
}

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct Bindings<'a> {
  #[serde(with = "indexmap::serde_seq")]
  inner: IndexMap<Cow<'a, str>, Vec<Type<'a>>>,
}

impl<'a> Bindings<'a> {
  pub fn new() -> Self {
    Self {
      inner: IndexMap::new(),
    }
  }

  pub fn insert(&mut self, name: Cow<'a, str>, ty: Type<'a>) {
    self
      .inner
      .entry(name.clone())
      .or_insert_with(Vec::new)
      .push(ty);
  }

  pub fn remove(&mut self, name: &str) {
    let is_empty = {
      let stack = self
        .inner
        .get_mut(name)
        .expect("attempted to remove an undefined binding");
      stack
        .pop()
        .expect("attempted to remove an undefined binding");
      stack.is_empty()
    };
    if is_empty {
      self.inner.remove(name);
    }
  }

  pub fn get(&self, name: &str) -> Option<&Type<'a>> {
    self.inner.get(name).and_then(|s| s.last())
  }
}

impl<'a> From<HashMap<Cow<'a, str>, Type<'a>>> for Bindings<'a> {
  fn from(map: HashMap<Cow<'a, str>, Type<'a>>) -> Self {
    Self {
      inner: map.into_iter().map(|(name, ty)| (name, vec![ty])).collect(),
    }
  }
}

impl<'a> FromIterator<(Cow<'a, str>, Type<'a>)> for Bindings<'a> {
  fn from_iter<T: IntoIterator<Item = (Cow<'a, str>, Type<'a>)>>(iter: T) -> Self {
    Self {
      inner: iter
        .into_iter()
        .map(|(name, ty)| (name, vec![ty]))
        .collect(),
    }
  }
}
