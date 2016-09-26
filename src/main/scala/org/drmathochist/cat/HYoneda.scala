package org.drmathochist.cat

import org.drmathochist.cat.HFunctor._

object HYoneda {
  type RepHNat[Phi[_[_]], F[_]] = ({type Nat[G[_]] = F~>G})#Nat ≈> Phi

  def toHObject[F[_]: Functor, Phi[_[_]]: HFunctor](hnat: RepHNat[Phi, F]): Phi[F] =
    hnat.t(Natural.id)

  def toHNatural[F[_]: Functor, Phi[_[_]]: HFunctor](phif: Phi[F]): RepHNat[Phi, F] =
    new RepHNat[Phi, F] {
      def t[G[_]: Functor](nat: F ~> G): Phi[G] = phif.hfmap(nat)
    }
}
