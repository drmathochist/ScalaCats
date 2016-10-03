package org.drmathochist.cat.functor

import Functor._

object Yoneda {
  type RepNat[F[_], A] = ({type Arr[B] = A=>B})#Arr ~> F
}

import Yoneda.RepNat

class Yoneda[A, F[_]: Functor] {
  // given a natural transformation from Rep[X] to F, gives an F[X]
  def toObject(nat: RepNat[F, A]): F[A] =
    nat.t(identity)

  // given an F[X], gives a natural transformation from Rep[X] to F
  def toNatural(fx: F[A]): RepNat[F, A] =
    new RepNat[F, A] {
      def t[B](arr: A => B): F[B] = fx.map(arr)
    }
}