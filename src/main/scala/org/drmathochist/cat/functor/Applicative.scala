package org.drmathochist.cat.functor

import org.drmathochist.cat.functor.Pointed.{RepresentablePointed, OptionIsPointed, ListIsPointed}

object Applicative {
  // adds an ap method to any instance of an applicative value
  class Ops[F[_]: Applicative, A](val self: F[A]) extends org.drmathochist.Ops[F[A]] {
    def ap[B](ff: F[A => B]): F[B] = implicitly[Applicative[F]].ap(self, ff)
  }
  implicit def toApplicativeOps[F[_]: Applicative, A](fa: F[A]) = new Ops(fa)


  // some examples of applicative functors
  // also useful as typeclass evidence objects
  implicit object ListIsApplicative extends ListIsApplicative
  trait ListIsApplicative extends ListIsPointed with Applicative[List] {
    def ap[A, B](as: List[A], fs: List[A => B]): List[B] =
      for { f <- fs; a <- as } yield f(a)
  }

  implicit object OptionIsApplicative extends OptionIsApplicative
  trait OptionIsApplicative extends OptionIsPointed with Applicative[Option] {
    def ap[A, B](opa: Option[A], opf: Option[A => B]): Option[B] =
      for { f <- opf; a <- opa } yield f(a)
  }

  implicit def representableApplicative[X] = new RepresentableApplicative[X] {}
  trait RepresentableApplicative[X] extends RepresentablePointed[X] with Applicative[({type F[A] = X=>A})#F] {
    // this is a re-ordered version of the S combinator!
    def ap[A, B](f: X => A, ff: X => A => B): X => B =
      x => ff(x)(f(x))
  }
}

trait Applicative[F[_]] extends Pointed[F] {
  def pure[A](a: => A): F[A] = point(a)

  def ap[A, B](fa: F[A], ff: F[A => B]): F[B]
}
