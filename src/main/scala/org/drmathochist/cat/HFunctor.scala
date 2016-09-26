package org.drmathochist.cat

object HFunctor {
  // adds a hmap method to any instance of a higher functor value
  class Ops[Phi[F[_]]: HFunctor, A[_]: Functor](val self: Phi[A])
    extends org.drmathochist.Ops[Phi[A]] {

    val HFPhi = implicitly[HFunctor[Phi]]
    val FA = implicitly[Functor[A]]

    def hfmap[B[_]: Functor](nat: A ~> B): Phi[B] =
      HFPhi.hfmap(nat)(FA, implicitly[Functor[B]])(self)
  }
  implicit def toHFunctorOps[Phi[F[_]]: HFunctor, A[_]: Functor](phia: Phi[A]) = new Ops(phia)

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
