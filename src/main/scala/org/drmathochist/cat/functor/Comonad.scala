package org.drmathochist.cat.functor


trait Comonad[CM[_]] extends Functor[CM] {
  def extract[A](cma: CM[A]): A
  def duplicate[A](cma: CM[A]): CM[CM[A]]
}
