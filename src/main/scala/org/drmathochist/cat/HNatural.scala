package org.drmathochist.cat

abstract class HNatural[Phi[F[_]]: HFunctor, Psi[F[_]]: HFunctor] {
  def t[F[_]: Functor](phif: Phi[F]): Psi[F]
}
