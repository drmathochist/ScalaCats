package org.drmathochist.cat.hfunctor

import org.drmathochist.cat.functor.{~>, Functor}
import org.drmathochist.cat._

object HNatural {
  /**
   * A higher natural transformation from the HFunctor represented by F to that represented by G.
   * By Yoneda's lemma (compare to class HYoneda) any such functor must arise from a natural transformation G ~> F.
   * given a natural transformation nat: G ~> F, we need a "natural" way of going from F ~> _ to G ~> _.
   *
   * That is, a family of functions: for every functor H, a function from (F ~> H) to (G ~> H).
   * Of course, this is just pre-composition by nat, which makes no reference at all to the functor H!
   *
s   * @param nat A natural transformation from G to F
   * @return The corresponding higher natural transformation from F ~> _ to G ~> _
   */
  def indnat[F[_]: Functor, G[_]: Functor](nat: G ~> F) =
    new (({type Hom[X[_]] = F~>X})#Hom ≈> ({type Hom[X[_]] = G~>X})#Hom) {
      def t[X[_]: Functor](phif: F ~> X): G ~> X = nat andThen phif
    }
}

/**
 * A "higher natural transformation" between two HFunctors is a function from Phi[F] to Psi[F], polymorphic in F.
 */
abstract class HNatural[Phi[F[_]]: HFunctor, Psi[F[_]]: HFunctor] {
  self =>

  /**
   * This is the "data" of a natural transformation: a family of functions, parameterized by
   * a functor F, such that t_F: Phi[F] => Psi[F].
   */
  def t[F[_]: Functor](phif: Phi[F]): Psi[F]

  /**
   * We can compose natural transformations.  If this one goes from Phi to Psi, and we get another
   * one from Psi to Xi, we can get one from Phi to Xi.
   */
  def andThen[Xi[_[_]]: HFunctor](nat: Psi ≈> Xi) = new (Phi ≈> Xi) {
    def t[F[_]: Functor](phif: Phi[F]): Xi[F] = nat.t(self.t(phif))
  }
}
