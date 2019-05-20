package org.drmathochist.lens

import org.drmathochist.cat.hfunctor.{Eval, EvalHYoneda}
import org.drmathochist.cat.hfunctor.HYoneda.RepHNat

object Lens {

  /**
   * A specialization of EvalHYoneda, setting F = Store[A, _] for some type A
   *
   * * (higher) natural transformations (Store[A, _] ~> G) ≈> Eval[S, G] = G[S]
   * * elements                         Eval[S, Store[A, _]] = Store[A, S]
   *
   */
  class Yoneda[A, B] extends EvalHYoneda[({type St[S] = Store[A, S]})#St, B]

  /**
   * The lens to access the first element of a pair
   */
  def first[A, B](p: (A, B)): Lens[A, (A, B)] = {
    new Lens(new Lens.Yoneda[A, (A, B)].toHNatural(Store[A, (A, B)](p._1, (a: A) => p.copy(_1 = a))))
  }

  /**
   * The lens to access the second element of a pair
   */
  def second[A, B](p: (A, B)): Lens[B, (A, B)] = {
    new Lens(new Lens.Yoneda[B, (A, B)].toHNatural(Store[B, (A, B)](p._2, (b: B) => p.copy(_2 = b))))
  }
}

/**
 * At a high level, we think of a lens as a way of "focusing" on one part of an overall structure.  We can evaluate
 * that part, or we can update the structure by replacing that part.
 *
 * In this implementation, we define a lens by a certain higher natural transformation.
 * The source HFunctor is Eval[S, _], which evaluates a functor at the type S of the overall structure.
 * The target HFunctor is Store[A, _] ~> _, which is the type of natural transformations from the functor Store[A, _]
 * to another given functor, and where A is the type of the piece of S under examination.
 *
 * The Yoneda lemma translates this natural transformation back and forth into a particular store object,
 * from which we can extract getters and setters as usual.  The advantage of using the natural transformation
 * as the defining data is that lenses compose exactly by composing their natural transformations!
 *
 * @param vL The natural transformation defining the lens
 * @tparam A The type of the "piece" the lens focuses on
 * @tparam S The type of the overall object the lens analyzes
 */
class Lens[A, S](val vL: RepHNat[({type Ev[X[_]]=Eval[S,X]})#Ev, ({type St[X]=Store[A,X]})#St]) {

  val ly = new Lens.Yoneda[A, S]

  val Store(g, s) = ly.toHObject(vL)
}