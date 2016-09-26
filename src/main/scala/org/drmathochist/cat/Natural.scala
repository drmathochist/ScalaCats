package org.drmathochist.cat

object Natural {
  def id[F[_]: Functor] = new (F ~> F) {
    def t[A]: F[A] => F[A] = identity
  }

  // an example of a natural transformation
  object HeadOption extends (List ~> Option) {
    def t[A]: List[A] => Option[A] = (fa: List[A]) => fa.headOption
  }
}

// A natural transformation from F to G is function from F[A] to G[A], polymorphic in A
abstract class Natural[F[_]: Functor, G[_]: Functor] {
  def t[A]: F[A] => G[A]
}
