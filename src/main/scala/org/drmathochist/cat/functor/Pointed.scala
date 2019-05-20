package org.drmathochist.cat.functor

import org.drmathochist.cat.functor.Functor.{RepresentableFunctor, OptionIsFunctor, ListIsFunctor}

object Pointed {

  /**
   * Adds a point method to any value
   * This seems tricky; which functor's point method should it add?  In practice,
   * the answer is determined from context by the desired type of the "wrapped" object.
   * @param self Any object
   */
  class Ops[A](val self: A) extends org.drmathochist.Ops[A] {
    /**
     * This will allow us to write a.point rather than pointedFunctorInfo.point(a).
     *
     * @tparam F The functor whose point method to add
     * @return The "wrapped" value of type F[A]
     */
    def point[F[_]: Pointed]: F[A] = implicitly[Pointed[F]].point(self)
  }
  implicit def toPointedOps[A](a: A) = new Ops(a)


  // some examples of pointed functors
  // also useful as typeclass evidence objects
  /**
   * The List functor is pointed
   */
  implicit object ListIsPointed extends ListIsPointed
  trait ListIsPointed extends ListIsFunctor with Pointed[List] {
    /**
     * The "natural" list built from an element a of A is the list containing exactly a.
     *
     * @param a Any object
     * @return The list containing exactly that one object
     */
    def point[A](a: => A): List[A] = List(a)
  }

  /**
   * The Option functor is pointed
   */
  implicit object OptionIsPointed extends OptionIsPointed
  trait OptionIsPointed extends OptionIsFunctor with Pointed[Option] {
    /**
     * The "natural" Option built from an element a of A is Some(a).
     * We use Option(a) here JUST IN CASE the result of evaluating a is null.
     *
     * @param a Any object
     * @return The Option containing that object, or None if the object is null
     */
    def point[A](a: => A): Option[A] = Option(a)
  }

  implicit def representablePointed[X] = new RepresentablePointed[X] {}
  // For any type X, the functor X => _ is pointed
  trait RepresentablePointed[X] extends RepresentableFunctor[X] with Pointed[({type F[A] = X=>A})#F] {
    // The "natural" function built from an element a of A is the constant function that always returns a.
    def point[A](a: => A): X=>A = _ => a
  }

  // Notice: Constant[X, _] is NOT usually pointed, as there is NO canonical way to
  // select an x from X given just an arbitrary element of some arbitrary type.
  // TODO: Exercise: If X is a monoid, Constant[X] IS pointed; why?
}

/**
 * A "pointed" functor also has a "natural" way of turning elements of A into elements of F[A]
 * for any type A.  This shows up in the form of a natural transformation -- that is,
 * a parametrically polymorphic family of functions from A to F[A] for each A.
 *
 * @tparam F a functor; this object provides the evidence that F is pointed
 */
trait Pointed[F[_]] extends Functor[F] {
  /**
   * The point operation takes an (unevaluated) element of A and returns a "wrapped" element in F[A].
   *
   * @param a any object
   * @return the same object, "wrapped" in the context of the functor F
   */
  def point[A](a: => A): F[A]
}
