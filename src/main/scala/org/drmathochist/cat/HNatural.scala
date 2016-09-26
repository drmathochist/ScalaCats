package org.drmathochist.cat

object HNatural {
  // a higher natural transformation from the HFunctor represented by F to that represented by G
  // by Yoneda's lemma (compare to HYoneda) any such functor must arise from a natural transformation G ~> F
  def indnat[F[_]: Functor, G[_]: Functor](nat: G ~> F) =
    new (({type Hom[X[_]] = F~>X})#Hom ≈> ({type Hom[X[_]] = G~>X})#Hom) {
      def t[X[_]: Functor](phif: F ~> X): G ~> X = nat andThen phif
    }
}

abstract class HNatural[Phi[F[_]]: HFunctor, Psi[F[_]]: HFunctor] {
  def t[F[_]: Functor](phif: Phi[F]): Psi[F]
}
