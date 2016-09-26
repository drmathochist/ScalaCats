package org.drmathochist.cat

object Functor {
  // adds a map method to any instance of a functor value
  class Ops[F[_], A](val self: F[A])(implicit F: Functor[F]) extends org.drmathochist.Ops[F[A]] {
    def map[B](f: A => B): F[B] = F.fmap(f)(self)
  }
  implicit def toFunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) = new Ops(fa)

  // some examples of functors
  // also useful as typeclass evidence objects
  implicit object ListIsFunctor extends Functor[List] {
    def fmap[A, B] = (f: A => B) => { (list: List[A]) => list.map(f) }
  }

  implicit object OptionIsFunctor extends Functor[Option] {
    def fmap[A, B] = f => op => op.map(f)
  }

  implicit def RepresentableFunctor[X] = new Functor[({type F[A] = X=>A})#F] {
    def fmap[A, B] = f => g => f compose g
  }
}

// a functor (from types to types) is a type constructor that can lift functions
trait Functor[F[_]] {
  def fmap[A, B]: (A => B) => (F[A] => F[B])
}

