package org.drmathochist

import org.drmathochist.cat.functor.Pointed

// a root trait for adding operations from typeclasses to objects
trait Ops[A] {
  def self: A
}
