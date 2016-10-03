package org.drmathochist.cat.hfunctor

import HFunctor._
import org.drmathochist.cat.functor.{~>, Natural, Functor}

object HYoneda {
  type RepHNat[Phi[_[_]], F[_]] = ({type Nat[G[_]] = F~>G})#Nat ≈> Phi
}

import HYoneda.RepHNat

/**
 * Given a functor F: * -> * and a higher-order functor Phi: (* -> *) -> * there is an isomorphism between
 *
 * * (higher) natural transformations F~>_ ≈> Phi[_]
 * * elements                         Phi[F]
 *
 * @tparam F    a (simple) functor
 * @tparam Phi  a higher-order functor
 */
class HYoneda[F[_]: Functor, Phi[_[_]]: HFunctor] {
  def toHObject(hnat: RepHNat[Phi, F]): Phi[F] =
    hnat.t(Natural.id)

  def toHNatural(phif: Phi[F]): RepHNat[Phi, F] =
    new RepHNat[Phi, F] {
      def t[G[_]: Functor](nat: F ~> G): Phi[G] = phif.hfmap(nat)
    }
}

/**
 * A specialization of Yoneda, setting Phi[G] = Eval[X, G] = G[X] for some type X
 * Given a functor F: * -> * there is an isomorphism between
 *
 * * (higher) natural transformations F~>_ ≈> Eval[X, _] = _[X]
 * * elements                         Eval[X, F] = F[X]
 *
 * @tparam F a functor
 * @tparam X a type to define an evaluation (higher-order) functor
 */
class EvalHYoneda[F[_]: Functor, X] extends HYoneda[F, ({type E[G[_]]=Eval[X, G]})#E]