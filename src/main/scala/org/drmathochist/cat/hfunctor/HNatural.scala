package org.drmathochist.cat.hfunctor

import org.drmathochist.cat.functor.{~>, Functor}
import org.drmathochist.cat._

object HNatural {
  // a higher natural transformation from the HFunctor represented by F to that represented by G
  // by Yoneda's lemma (compare to HYoneda) any such functor must arise from a natural transformation G ~> F
  def indnat[F[_]: Functor, G[_]: Functor](nat: G ~> F) =
    new (({type Hom[X[_]] = F~>X})#Hom ≈> ({type Hom[X[_]] = G~>X})#Hom) {
      def t[X[_]: Functor](phif: F ~> X): G ~> X = nat andThen phif
    }
}

abstract class HNatural[Phi[F[_]]: HFunctor, Psi[F[_]]: HFunctor] {
  self =>

  def t[F[_]: Functor](phif: Phi[F]): Psi[F]

  def andThen[Xi[_[_]]: HFunctor](nat: Psi ≈> Xi) = new (Phi ≈> Xi) {
    def t[F[_]: Functor](phif: Phi[F]): Xi[F] = nat.t(self.t(phif))
  }
}
