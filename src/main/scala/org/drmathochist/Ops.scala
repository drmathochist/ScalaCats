package org.drmathochist

// a root trait for adding operations from typeclasses to objects
trait Ops[A] {
  def self: A
}
