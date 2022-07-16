import module
import module.A
import module.{A, B}

import nested.module.A
import nested.module.{A, B}

import {
  a.{A, B},
  b.{C, D},
  nested.{
    d.{E, F},
    f.{G, H},
  }
}

// you can export functions, classes, enums, and types
export fn a() {}
export class B {}
export enum C {}
export type D {}

// exports may also be grouped into a block export
fn e() {}
class F {}
enum G {}
type H {}
// symbols exported this way may be renamed using `as`
export { e as h, F as G, G as F, H as E }