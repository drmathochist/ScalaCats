package org.drmathochist.cat

object Functor {
  // adds a map method to any instance of a functor value
  class Ops[F[_]: Functor, A](val self: F[A]) extends org.drmathochist.Ops[F[A]] {
    def map[B](f: A => B): F[B] = implicitly[Functor[F]].fmap(self, f)
  }
  implicit def toFunctorOps[F[_]: Functor, A](fa: F[A]) = new Ops(fa)

  // some examples of functors
  // also useful as typeclass evidence objects
  implicit object ListIsFunctor extends Functor[List] {
    def fmap[A, B](list: List[A], f: A => B) = list.map(f)
  }

  implicit object OptionIsFunctor extends Functor[Option] {
    def fmap[A, B](op: Option[A], f: A => B) = op.map(f)
  }

  implicit def RepresentableFunctor[X] = new Functor[({type F[A] = X=>A})#F] {
    def fmap[A, B](g: X => A, f: A => B) = f compose g
  }
}

// a functor (from types to types) is a type constructor that can lift functions
trait Functor[F[_]] {
  def fmap[A, B](fa: F[A], arr: A => B): F[B]

  def lift[A, B](arr: A => B): F[A] => F[B] = fa => fmap(fa, arr)
}

