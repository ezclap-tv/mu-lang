use module;
use module.A;
use module.{A, B};

use nested.module.A;
use nested.module.{A, B};

use {
  a.{A, B},
  b.{C, D},
  nested.{
    d.{E, F},
    f.{G, H},
  }
};

pub fn a() {}
pub class B {}
pub trait C {}
pub type D = B;
