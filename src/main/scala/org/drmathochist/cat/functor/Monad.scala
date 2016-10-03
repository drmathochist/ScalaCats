package org.drmathochist.cat.functor

import org.drmathochist.cat.functor.Applicative.{RepresentableApplicative, OptionIsApplicative, ListIsApplicative}

object Monad {
  // adds an ap method to any instance of an applicative value
  class Ops[M[_]: Monad, A](val self: M[A]) extends org.drmathochist.Ops[M[A]] {
    def flatmap[B](f: A => M[B]): M[B] = implicitly[Monad[M]].flatmap(self, f)
  }
  implicit def toMonadOps[M[_]: Monad, A](ma: M[A]) = new Ops(ma)


  // some examples of applicative functors
  // also useful as typeclass evidence objects
  implicit object ListIsMonad extends ListIsMonad
  trait ListIsMonad extends ListIsApplicative with Monad[List] {
    def flatmap[A, B](as: List[A], f: A => List[B]): List[B] =
      as.flatMap(f)
  }

  implicit object OptionIsMonad extends OptionIsMonad
  trait OptionIsMonad extends OptionIsApplicative with Monad[Option] {
    def flatmap[A, B](opa: Option[A], f: A => Option[B]): Option[B] =
      opa.flatMap(f)
  }

  implicit def representableMonad[X] = new RepresentableMonad[X] {}
  trait RepresentableMonad[X] extends RepresentableApplicative[X] with Monad[({type F[A] = X=>A})#F] {
    // this is a re-ordered version of the S combinator!
    def flatmap[A, B](f: X => A, ff: A => X => B): X => B =
      x => ff(f(x))(x)
  }
}

trait Monad[M[_]] extends Applicative[M] {
  def unit[A](a: => A): M[A] = point(a)

  def flatmap[A, B](ma: M[A], f: A => M[B]): M[B]

  def flatten[A](mma: M[M[A]]): M[A] = flatmap(mma, identity[M[A]])
}
