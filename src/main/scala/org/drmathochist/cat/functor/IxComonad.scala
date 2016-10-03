package org.drmathochist.cat.functor

trait IxComonad[W[_, _, _]] {
  def iextract[A, T](waat: W[A, A, T]): T
  def iduplicate[A, B, J, T](wabt: W[A, B, T]): W[A, J, W[J, B, T]]
}
