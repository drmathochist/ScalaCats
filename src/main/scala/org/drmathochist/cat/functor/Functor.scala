package org.drmathochist.cat.functor

import org.drmathochist.cat._

object Functor {
  // adds a map method to any instance of a functor value
  class Ops[F[_]: Functor, A](val self: F[A]) extends org.drmathochist.Ops[F[A]] {
    def map[B](f: A => B): F[B] = implicitly[Functor[F]].fmap(self, f)
  }
  implicit def toFunctorOps[F[_]: Functor, A](fa: F[A]) = new Ops(fa)

  // some examples of functors
  // also useful as typeclass evidence objects
  implicit object ListIsFunctor extends ListIsFunctor
  trait ListIsFunctor extends Functor[List] {
    def fmap[A, B](list: List[A], f: A => B) = list.map(f)
  }

  implicit object OptionIsFunctor extends OptionIsFunctor
  trait OptionIsFunctor extends Functor[Option] {
    def fmap[A, B](op: Option[A], f: A => B) = op.map(f)
  }

  implicit def representableFunctor[X] = new RepresentableFunctor[X] {}
  trait RepresentableFunctor[X] extends Functor[({type F[A] = X=>A})#F] {
    def fmap[A, B](g: X => A, f: A => B) = f compose g
  }

  implicit def constantFunctor[X] = new ConstantFunctor[X] {}
  trait ConstantFunctor[X] extends Functor[({type C[A] = Constant[X, A]})#C] {
    def fmap[A, B](x: X, f: A => B) = x
  }
}

// a functor (from types to types) is a type constructor that can lift functions
trait Functor[F[_]] {
  def fmap[A, B](fa: F[A], arr: A => B): F[B]

  def lift[A, B](arr: A => B): F[A] => F[B] = fa => fmap(fa, arr)
}
