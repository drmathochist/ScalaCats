package org.drmathochist.cat

object HFunctor {
  // adds a hmap method to any instance of a higher functor value
  class Ops[Phi[_[_]]: HFunctor, F[_]: Functor](val self: Phi[F]) extends org.drmathochist.Ops[Phi[F]] {
    def hfmap[G[_]: Functor](nat: F ~> G): Phi[G] = implicitly[HFunctor[Phi]].hfmap(self, nat)
  }
  implicit def toHFunctorOps[Phi[_[_]]: HFunctor, F[_]: Functor](phif: Phi[F]) = new Ops(phif)

  implicit def RepresentableHFunctor[X[_]: Functor] = new HFunctor[({type Hom[F[_]] = X~>F})#Hom] {
    def hfmap[F[_]: Functor, G[_]: Functor](nu: X ~> F, nat: F ~> G) = nu andThen nat
  }
}

// Phi should have kind (* -> *) -> *
// meaning HFunctor should have kind ((* -> *) -> *) -> *
trait HFunctor[Phi[F[_]]] {
  def hfmap[F[_]: Functor, G[_]: Functor](phif: Phi[F], nat: F ~> G): Phi[G]

  def lift[F[_]: Functor, G[_]: Functor](nat: F ~> G): Phi[F] => Phi[G] = phif => hfmap(phif, nat)
}
