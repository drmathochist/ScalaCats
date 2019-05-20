package org.drmathochist.cat.functor

import org.drmathochist.cat._

object Natural {
  /**
   * For every functor F, there is an "identity" natural transformation id: F ~> F
   * that just acts as the identity on each F[A]!
   */
  def id[F[_]: Functor] = new (F ~> F) {
    def t[A](fa: F[A]): F[A] = fa
  }

  /**
   * An example of a natural transformation: for every type A, we have a function
   * from List[A] to Option[A] that extracts the head element of the list, or
   * returns None for an empty list.  This is "natural" in that we define the
   * function without any reference to the type A itself.
   */
  object HeadOption extends (List ~> Option) {
    def t[A](list: List[A]): Option[A] = list.headOption
  }

  /**
   * A natural transformation from the Functor represented by A to that represented by B.
   * By Yoneda's lemma (compare to class Yoneda) any such functor must arise from a function B => A.
   *
   * Given a function arr: B => A, we need a "natural" way of going from A => _ to B => _.
   * That is, a family of functions: for every type X, a function from (A => X) to (B => X).
   * Of course, this is just pre-composition by arr, which makes no reference at all to the type X!
   *
   * @param arr A function from B to A.
   * @return The corresponding natural transformation from A => _ to B => _
   */
  def indnat[A, B](arr: B => A) =
    new (({type Arr[X] = A=>X})#Arr ~> ({type Arr[X] = B=>X})#Arr) {
      def t[X](f: A => X): B => X = arr andThen f
    }
}

/**
 * A natural transformation from F to G is function from F[A] to G[A], polymorphic in A.
 *
 * @tparam F The source functor of the natural transformation
 * @tparam G The target functor of the natural transformation
 */
abstract class Natural[F[_]: Functor, G[_]: Functor] {
  self =>

  /**
   * This is the "data" of a natural transformation: a family of functions, parameterized by
   * a type A, such that t_A: F[A] => G[A].
   */
  def t[A](fa: F[A]): G[A]

  /**
   * We can compose natural transformations.  If this one goes from F to G, and we get another
   * one from G to H, we can get one from F to H.
   *
   * @param nat Another natural transformation, whose source functor is G, the target of this one.
   * @tparam H The target functor of nat.
   * @return The composed natural transformation from F to H.
   */
  def andThen[H[_]: Functor](nat: G ~> H) = new (F ~> H) {
    def t[A](fa: F[A]): H[A] = nat.t(self.t(fa))
  }
}
