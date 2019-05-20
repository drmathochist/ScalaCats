package org.drmathochist.cat.functor

import Functor._

object Yoneda {
  /**
   * Defines some shorthand: RepNat[F[_], A] is the type of a natural transformation
   * which goes from the functor A => _ "represented by A" to the functor F[_].
   *
   * @tparam F The target functor of the natural transformations in this type
   * @tparam A The "representing object" of the source functor
   */
  type RepNat[F[_], A] = ({type Arr[B] = A=>B})#Arr ~> F
}

import Yoneda.RepNat

/**
 * The Yoneda Lemma states that there is a natural bijective correspondence
 * between elements of F[A] and natural transformations from A => _ to F[_].
 *
 *
 * This object defines both directions of this bijection.  That they are
 * inverses is the content of the lemma, but amounts to a straightforward
 * unwinding of all the definitions.
 * The upshot is that any time we see a polymorphic function that takes
 * a function A => X and gives an F[X] for any type X, it MUST be
 * represented by some unique object in F[A].
 */
class Yoneda[A, F[_]: Functor] {
  /**
   * Given a natural transformation from A => _ to F, gives an F[A]
   * If nat is such a natural transformation, we can always just apply it to
   * the identity function on A itself.
   *
   * @param nat A natural transformation from the functor represented by A to F.
   * @return The corresponding element of F[A].
   */
  def toObject(nat: RepNat[F, A]): F[A] =
    nat.t(identity)

  /**
   * Given an F[A], gives a natural transformation from A => _ to F
   * This will take any function from A to B and fmap it over the given fa.
   *
   * @param fa An element of F[A]
   * @return The corresponding natural transformation from A => _ to F
   */
  def toNatural(fa: F[A]): RepNat[F, A] =
    new RepNat[F, A] {
      def t[B](arr: A => B): F[B] = fa.map(arr)
    }
}