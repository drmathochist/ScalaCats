package org.drmathochist.cat.functor

import org.drmathochist.cat._

object Natural {
  def id[F[_]: Functor] = new (F ~> F) {
    def t[A](fa: F[A]): F[A] = fa
  }

  // an example of a natural transformation
  object HeadOption extends (List ~> Option) {
    def t[A](list: List[A]): Option[A] = list.headOption
  }

  // a natural transformation from the Functor represented by F to that represented by G
  // by Yoneda's lemma (compare to Yoneda) any such functor must arise from a function G => F
  def indnat[A, B](arr: B => A) =
    new (({type Arr[X] = A=>X})#Arr ~> ({type Arr[X] = B=>X})#Arr) {
      def t[X](f: A => X): B => X = arr andThen f
    }
}

// A natural transformation from F to G is function from F[A] to G[A], polymorphic in A
abstract class Natural[F[_]: Functor, G[_]: Functor] {
  self =>

  def t[A](fa: F[A]): G[A]

  def andThen[H[_]: Functor](nat: G ~> H) = new (F ~> H) {
    def t[A](fa: F[A]): H[A] = nat.t(self.t(fa))
  }
}
