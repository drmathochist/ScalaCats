package org.drmathochist.cat.functor

import org.drmathochist.cat._

object Functor {
  /**
   * Adds a map method to any instance of a functor value
   *
   * @tparam F A functor
   * @tparam A A base type
   * @param self A value of type F[A], which will gain a map method
   */
  class Ops[F[_]: Functor, A](val self: F[A]) extends org.drmathochist.Ops[F[A]] {
    /**
     * This will allow us to write fa.map(f) rather than functorInfo.fmap(fa, f)
     *
     * @param f A function to apply
     * @return the result of applying f, lifted to the context of F.
     */
    def map[B](f: A => B): F[B] = implicitly[Functor[F]].fmap(self, f)
  }
  implicit def toFunctorOps[F[_]: Functor, A](fa: F[A]) = new Ops(fa)

  // some examples of functors
  // also useful as typeclass evidence objects

  /**
   * The type constructor List[_] is a functor
   */
  implicit object ListIsFunctor extends ListIsFunctor
  trait ListIsFunctor extends Functor[List] {
    /**
     * Functions lift using the usual map over lists.
     *
     * @param list A list of elements of A
     * @param f A function to apply to each list element
     * @return The list of function outputs
     */
    def fmap[A, B](list: List[A], f: A => B) = list.map(f)
  }

  /**
   * The type constructor Option[_] is a functor/
   */
  implicit object OptionIsFunctor extends OptionIsFunctor
  trait OptionIsFunctor extends Functor[Option] {
    /**
     * Functions lift using the usual map over objects.
     *
     * @param op An optional value of type A
     * @param f A function to apply if the optional value is defined
     * @return The result of applying the function, or None if the option is undefined
     */
    def fmap[A, B](op: Option[A], f: A => B) = op.map(f)
  }

  /**
   * For any X, the type constructor X => _ is a functor
   * @tparam X a "representing object" type
   */
  implicit def representableFunctor[X] = new RepresentableFunctor[X] {}
  trait RepresentableFunctor[X] extends Functor[({type F[A] = X=>A})#F] {
    /**
     * functions lift using composition!
     *
     * @param g The function from X to map over
     * @param f The function to apply
     * @tparam A The source type of f
     * @tparam B The target type of f
     * @return The composition of f and g, now a function from X to B.
     */
    def fmap[A, B](g: X => A, f: A => B) = f compose g
  }

  /**
   * For any X, the type constructor Constant[X, _] is a functor
   * @tparam X The constant type
   */
  implicit def constantFunctor[X] = new ConstantFunctor[X] {}
  trait ConstantFunctor[X] extends Functor[({type C[A] = Constant[X, A]})#C] {
    /**
     * functions lift by acting trivially, doing nothing at all!
     *
     * @param x the value to "map over", considered as an element of Constant[X, A] = X
     * @param f the function to "apply"
     * @return just x again, but now considered as an element of Constant[X, B] = X
     */
    def fmap[A, B](x: X, f: A => B) = x
  }
}

/**
 * A functor (from types to types) is a type constructor that can lift functions
 * These are more specific than "functors" in the categorical sense, being the
 * particular case where the source and target categories are both that of Scala types.
 *
 * @tparam F a type constructor; this object provides the evidence that F is a functor
 */
trait Functor[F[_]] {
  /**
   * The fmap operation takes an element fa in F[A] and a function arr: A => B
   * and applies arr "inside the functor" to give an element of F[B].
   *
   * @param fa A value of type F[A]
   * @param arr A function from A to B
   * @return A value of type F[B]
   */
  def fmap[A, B](fa: F[A], arr: A => B): F[B]

  /**
   * lift uses fmap to turn a function A => B into a function F[A] => F[B]
   *
   * @param arr A function from A to B
   * @return The "lifted" function from F[A] to F[B]
   */
  def lift[A, B](arr: A => B): F[A] => F[B] = fa => fmap(fa, arr)
}
