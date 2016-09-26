package org.drmathochist.cat

import org.drmathochist.cat.Functor._

object Yoneda {
  type RepNat[F[_], A] = ({type Arr[B] = A=>B})#Arr ~> F

  // given a natural transformation from Rep[X] to F, gives an F[X]
  def toObject [A, F[_]: Functor](nat: RepNat[F, A]): F[A] =
    nat.t(identity)

  // given an F[X], gives a natural transformation from Rep[X] to F
  def toNatural[A, F[_]: Functor](fx: F[A]): RepNat[F, A] =
    new RepNat[F, A] {
      def t[B](arr: A => B): F[B] = fx.map(arr)
    }
}
