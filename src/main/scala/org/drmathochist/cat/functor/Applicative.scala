package org.drmathochist.cat.functor

import org.drmathochist.cat.functor.Pointed.{RepresentablePointed, OptionIsPointed, ListIsPointed}

object Applicative {

  /**
   * Adds an ap method to any instance of an applicative value
   *
   * @param self the functor value that should carry an ap method
   */
  class Ops[F[_]: Applicative, A](val self: F[A]) extends org.drmathochist.Ops[F[A]] {
    /**
     * This will allow us to write fa.ap(ff) rather than applicativeFunctorInfo.ap(fa, ff)
     *
     * @param ff a function value "wrapped" in the same functor context as the self object
     * @return The result of applying the function "inside the functor"
     */
    def ap[B](ff: F[A => B]): F[B] = implicitly[Applicative[F]].ap(self, ff)
  }
  implicit def toApplicativeOps[F[_]: Applicative, A](fa: F[A]) = new Ops(fa)


  // some examples of applicative functors
  // also useful as typeclass evidence objects

  /**
   * The List functor is applicative
   */
  implicit object ListIsApplicative extends ListIsApplicative
  trait ListIsApplicative extends ListIsPointed with Applicative[List] {
    /**
     * Apply a list of functions to a list of elements by applying each function to each element.
     * The order matters here; the elements are in the "tight" loop,
     * so all the applications of a given function are together in the resulting list.
     *
     * @param as A list of elements of A
     * @param fs A list of functions from A to B
     * @return The list of values in B of each function applied to each of the list elements
     */
    def ap[A, B](as: List[A], fs: List[A => B]): List[B] =
      for { f <- fs; a <- as } yield f(a)
  }

  /**
   * The Option functor is applicative
   */
  implicit object OptionIsApplicative extends OptionIsApplicative
  trait OptionIsApplicative extends OptionIsPointed with Applicative[Option] {
    /**
     * Apply an optional function to an optional value, returning None if either is missing.
     *
     * @param opa An optional value
     * @param opf An optional function to apply
     * @return The optional result of applying the function to the value, or None if either are undefined
     */
    def ap[A, B](opa: Option[A], opf: Option[A => B]): Option[B] =
      for { f <- opf; a <- opa } yield f(a)
  }

  // For both List and Option, we kind of cheat by using the fact that these types are already "monadic",
  // in that they have the requisite operations for the Scala for-comprehension sugar to work.
  // This is okay, though, since every Monad is Applicative, and in exactly this way.

  /**
   * For any type X, the functor X => _ is applicative
   * @tparam X the "representing type" of the functor
   */
  implicit def representableApplicative[X] = new RepresentableApplicative[X] {}
  trait RepresentableApplicative[X] extends RepresentablePointed[X] with Applicative[({type F[A] = X=>A})#F] {
    /**
     * If we can use an x in X to build both a function ff(x): A => B and an element f(x) in A,
     * then we can just apply that function to that element to get an element of B.
     *
     * This is secretly just a re-ordered version of the S combinator!
     *
     * @param f
     * @param ff A function that builds a function A => B from an element of X
     * @return A function that builds an element of B from an element of X by applying the result of ff to that of f
     */
    def ap[A, B](f: X => A, ff: X => A => B): X => B =
      x => ff(x)(f(x))
  }
}

/**
 * An "applicative" functor is pointed, and also allows for applying functions "inside the functor".
 * This also allows for lifting curried multi-argument functions.
 *
 * @tparam F a pointed functor; this object provides the evidence that F is applicative
 */
trait Applicative[F[_]] extends Pointed[F] {
  /**
   * We usually call the point operation "pure" in the context of applicative functors.
   */
  def pure[A](a: => A): F[A] = point(a)

  /**
   * The ap operation takes an element fa in F[A] and a "wrapped" function ff: F[A => B]
   * and "applies" the function as if "unwrapping" both a value a in A and a function f: A => B,
   * evaluating the function f(a) in B, and then "re-wrapping" the result into an element of F[B].
   *
   * Of course, "unwrapping" isn't usually possible, but that's the effective idea.
   *
   * @param fa A "wrapped" A-value in F[A]
   * @param ff A function A=>B "wrapped" in the same F-context as fa
   * @return The "wrapped" result of applying the "unwrapped" function to the "unwrapped" value
   */
  def ap[A, B](fa: F[A], ff: F[A => B]): F[B]
}
