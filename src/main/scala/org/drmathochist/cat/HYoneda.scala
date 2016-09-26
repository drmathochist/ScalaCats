package org.drmathochist.cat

import org.drmathochist.cat.HFunctor._

object HYoneda {
  type RepHNat[Phi[X[_]], F[_]] = ({type Nat[A[_]] = F~>A})#Nat ≈> Phi

  def toHObject[F[_]: Functor, Phi[X[_]]: HFunctor](hnat: RepHNat[Phi, F]): Phi[F] =
    hnat.t(Natural.id)

  def toHNatural[A[_]: Functor, Phi[F[_]]: HFunctor](phia: Phi[A]): RepHNat[Phi, A] =
    new RepHNat[Phi, A] {
      def t[B[_]: Functor](nat: A ~> B): Phi[B] = phia.hfmap(nat)
    }
}
