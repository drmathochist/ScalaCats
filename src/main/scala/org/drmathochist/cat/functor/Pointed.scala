package org.drmathochist.cat.functor

import org.drmathochist.cat.functor.Functor.{RepresentableFunctor, OptionIsFunctor, ListIsFunctor}

object Pointed {
  // adds a point method to any value
  class Ops[A](val self: A) extends org.drmathochist.Ops[A] {
    def point[F[_]: Pointed]: F[A] = implicitly[Pointed[F]].point(self)
  }
  implicit def toPointedOps[A](a: A) = new Ops(a)


  // some examples of pointed functors
  // also useful as typeclass evidence objects
  implicit object ListIsPointed extends ListIsPointed
  trait ListIsPointed extends ListIsFunctor with Pointed[List] {
    def point[A](a: => A): List[A] = List(a)
  }

  implicit object OptionIsPointed extends OptionIsPointed
  trait OptionIsPointed extends OptionIsFunctor with Pointed[Option] {
    def point[A](a: => A): Option[A] = Option(a)
  }

  implicit def representablePointed[X] = new RepresentablePointed[X] {}
  trait RepresentablePointed[X] extends RepresentableFunctor[X] with Pointed[({type F[A] = X=>A})#F] {
    def point[A](a: => A): X=>A = _ => a
  }
}

trait Pointed[F[_]] extends Functor[F] {
  def point[A](a: => A): F[A]
}
