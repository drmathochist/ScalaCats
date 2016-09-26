package org.drmathochist.cat

object HFunctor {
  implicit def RepresentableHFunctor[X[_]: Functor] = new HFunctor[({type Hom[F[_]] = X~>F})#Hom] {
    def hfmap[F[_]: Functor, G[_]: Functor](nat: F ~> G) = (nat2: X ~> F) => new (X ~> G) {
      def t[A]: X[A] => G[A] = nat2.t andThen nat.t
    }
  }
}

// Phi should have kind (* -> *) -> *
// meaning HFunctor should have kind ((* -> *) -> *) -> *
trait HFunctor[Phi[F[_]]] {
  def hfmap[F[_]: Functor, G[_]: Functor](nat: F ~> G): Phi[F] => Phi[G]
}
