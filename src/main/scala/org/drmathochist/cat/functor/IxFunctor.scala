package org.drmathochist.cat.functor

trait IxFunctor[F[_, _, _]] {
  def imap[A, B, S, T](fabs: F[A, B, S], f: S => T): F[A, B, T]
}
