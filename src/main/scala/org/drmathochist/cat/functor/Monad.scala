package org.drmathochist.cat.functor

import org.drmathochist.cat.functor.Applicative.{RepresentableApplicative, OptionIsApplicative, ListIsApplicative}

object Monad {

  /**
   * Adds an ap method to any instance of an applicative value
   * @param self
   * @param ev$1
   * @tparam M
   * @tparam A
   */
  class Ops[M[_]: Monad, A](val self: M[A]) extends org.drmathochist.Ops[M[A]] {
    def flatmap[B](f: A => M[B]): M[B] = implicitly[Monad[M]].flatmap(self, f)
  }
  implicit def toMonadOps[M[_]: Monad, A](ma: M[A]) = new Ops(ma)


  // some examples of applicative functors
  // also useful as typeclass evidence objects

  /**
   * The List functor is a monad
   */
  implicit object ListIsMonad extends ListIsMonad
  trait ListIsMonad extends ListIsApplicative with Monad[List] {
    /**
     * The flatmap operation for lists is well-known: map f over the list of as,
     * then concatenate all the resulting lists!
     *
     * @param as A list of elements of A
     * @param f A function that returns a list of elements of B for each element of A
     * @return The list of all elements of B produced by applying f to each list element
     */
    def flatmap[A, B](as: List[A], f: A => List[B]): List[B] =
      as.flatMap(f)
  }

  /**
   * The Option functor is a monad
   */
  implicit object OptionIsMonad extends OptionIsMonad
  trait OptionIsMonad extends OptionIsApplicative with Monad[Option] {
    /**
     * If opa is already None, just return None, otherwise apply f to the value.
     *
     * @param opa An optional value in A
     * @param f A function that returns an optional value in B
     * @return the result of applying f to the value, or None if the value is undefined
     */
    def flatmap[A, B](opa: Option[A], f: A => Option[B]): Option[B] =
      opa.flatMap(f)
  }

  /**
   * For any type X, the functor X => _ is a monad
   *
   * @tparam X A "representing type"
   */
  implicit def representableMonad[X] = new RepresentableMonad[X] {}
  trait RepresentableMonad[X] extends RepresentableApplicative[X] with Monad[({type F[A] = X=>A})#F] {
    /**
     * First, use x to build an element a in A.  Then use both a and x in the function ff to get an element of B.
     *
     * This is secretly just ANOTHER re-ordered version of the S combinator!
     *
     * @param f A function that builds an element of A from an element of X
     * @param ff Given a value from A, returns a function that builds an element of B from an element of X
     */
    def flatmap[A, B](f: X => A, ff: A => X => B): X => B =
      x => ff(f(x))(x)
  }
}

/**
 * A "monadic" functor, or "monad" provides a way of composing functions of the form A => F[B].
 * This allows for more general notions of computation, where we commonly
 * interpret A => F[B] as a function from A to B "with side-effects" defined
 * by F.  It may not return a value (Option), or may return a non-deterministic
 * value (List), or rely on "reading" a configuration object X (X => _).
 *
 * @tparam M An applicative functor; this object provides the evidence that M is a monad
 */
trait Monad[M[_]] extends Applicative[M] {
  /**
   * We usually call the point operation "unit" in the context of monads
   */
  def unit[A](a: => A): M[A] = point(a)

  /**
   * The flatmap operation takes an element ma in M[A] and a function f: A => M[B], and returns
   * an element of M[B].  This differs from ap in that flatmap can "look at" the element of A directly.
   *
   * Since M is a functor, this is effectively the same as using fmap to apply f "inside M" to get
   * a value in M[M[B]], then using flatten to get a value in M[B].  Thus: flat-map.
   *
   * @param ma A "monadic value" of type M[A]
   * @param f A function that takes a value in A and returns a "monadic value" in M[B]
   * @return The result of applying f "inside the monad".
   */
  def flatmap[A, B](ma: M[A], f: A => M[B]): M[B]

  /**
   * The flatten operation takes an element of M[M[A]] and "flattens" its type into M[A].
   * This is logically equivalent to flatmap, and either can be defined in terms of the other.
   *
   * From a mathematical viewpoint, this is the more conceptually primitive operation, being a
   * "natural transformation" from the functor M[M[_]] to the functor M[_], in a way
   * "composing" the two functor applications into one.  But the computational interpretation
   * makes flatmap the more useful one for programming.
   *
   * @param mma A value of type M[M[A]]
   * @return The "flattened" value of type M[A]
   */
  def flatten[A](mma: M[M[A]]): M[A] = flatmap(mma, identity[M[A]])
}
