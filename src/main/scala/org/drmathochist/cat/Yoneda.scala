package org.drmathochist.cat

import org.drmathochist.cat.Functor._

object Yoneda {
  type RepNat[F[_], X] = ({type Arr[A] = X=>A})#Arr ~> F

  // given a natural transformation from Rep[X] to F, gives an F[X]
  def toObject [X, F[_]: Functor](nat: RepNat[F, X]): F[X] =
    nat.t(identity)

  // given an F[X], gives a natural transformation from Rep[X] to F
  def toNatural[X, F[_]: Functor](fx: F[X]): RepNat[F, X] =
    new RepNat[F, X] {
      def t[A] = arr => fx.map(arr)
    }
}
