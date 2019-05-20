package org.drmathochist.cat.hfunctor

import org.drmathochist.cat.functor.{~>, Functor}

object HFunctor {

  /**
   * Adds a hmap method to any instance of a higher functor value
   *
   * @param self A value of Phi[F], which will gain an hfmap method
   */
  //
  class Ops[Phi[_[_]]: HFunctor, F[_]: Functor](val self: Phi[F]) extends org.drmathochist.Ops[Phi[F]] {
    /**
     * This will allow us to write phif.map(nat) rather than hfunctorInfo.fmap(phif, nat)
     */
    def hfmap[G[_]: Functor](nat: F ~> G): Phi[G] = implicitly[HFunctor[Phi]].hfmap(self, nat)
  }
  implicit def toHFunctorOps[Phi[_[_]]: HFunctor, F[_]: Functor](phif: Phi[F]) = new Ops(phif)

  /**
   * This will allow us to write phif.map(nat) rather than hfunctorInfo.fmap(phif, nat).
   */
  implicit def RepresentableHFunctor[X[_]: Functor] = new HFunctor[({type Hom[F[_]] = X~>F})#Hom] {
    /**
     * Natural transformations lift using composition!
     */
    def hfmap[F[_]: Functor, G[_]: Functor](nu: X ~> F, nat: F ~> G) = nu andThen nat
  }

  /**
   * For any type X, the type constructor HConstant[X, _] is an HFunctor
   */
  implicit def ConstantHFunctor[X] = new HFunctor[({type C[F[_]] = HConstant[X, F]})#C] {
    /**
     * Natural transformations lift by acting trivially, doing nothing at all!
     */
    def hfmap[F[_]: Functor, G[_]: Functor](x: X, nat: F ~> G) = x
  }

  /**
   * For any type X, the type constructor Eval[X, _] is an HFunctor.
   * That is, evaluating functors at a fixed type X is itself functorial!
   *
   * This is probably the most important source of HFunctors in the wild.
   */
  implicit def EvalHFunctor[X] = new HFunctor[({type E[F[_]] = Eval[X, F]})#E] {
    /**
     * For each functor F, the evaluation at X is just Eval[X, F] = F[X].
     * Thus, given a natural transformation nat F ~> G, we can focus on the
     * single component nat.t[X]: F[X] => G[X].
     */
    def hfmap[F[_]: Functor, G[_]: Functor](fx: F[X], nat: F ~> G) = nat.t(fx)
  }
}

/**
 * A "higher functor" is a functor from the category of ("regular") functors to that of types,
 * where the arrows between functors are natural transformations.  Such an HFunctor, typically
 * called Phi, is defined by a type constructor that takes a functor F and returns a type Phi[F].
 * This is exactly analogous to the definition in Functor, but with functors taking the place of
 * types, and natural transformations taking the place of functions.  We can thus "lift"
 * natural transformations to act "inside" HFunctors.
 *
 * An HFunctor Phi should have kind (* -> *) -> *
 * meaning the HFunctor typeclass should have kind ((* -> *) -> *) -> *
 *
 * @tparam Phi A "higher" type constructor of kind (* -> *) -> *
 */
trait HFunctor[Phi[F[_]]] {
  /**
   * The hfmap operation takes an element pf in Phi[F] and a natural transformation nat: F ~> G
   * and applies nat "inside the higher functor" to give an element of Phi[G].
   *
   * @param phif An element of the type Phi[F]
   * @param nat A natural transformation from F to G
   */
  def hfmap[F[_]: Functor, G[_]: Functor](phif: Phi[F], nat: F ~> G): Phi[G]

  /**
   * lift uses hfmap to turn a natural transformation F ~> G into a function Phi[F] => Phi[G]
   *
   * @param nat A natural transformation from F to G
   * @return The "lifted" function from Phi[F] to Phi[G]
   */
  def lift[F[_]: Functor, G[_]: Functor](nat: F ~> G): Phi[F] => Phi[G] = phif => hfmap(phif, nat)
}
