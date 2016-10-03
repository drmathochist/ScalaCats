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

  def first[A, B](p: (A, B)): Lens[A, (A, B)] = {
    new Lens(new Lens.Yoneda[A, (A, B)].toHNatural(Store[A, (A, B)](p._1, (a: A) => p.copy(_1 = a))))
  }

  def second[A, B](p: (A, B)): Lens[B, (A, B)] = {
    new Lens(new Lens.Yoneda[B, (A, B)].toHNatural(Store[B, (A, B)](p._2, (b: B) => p.copy(_2 = b))))
  }
}

class Lens[A, S](val vL: RepHNat[({type Ev[X[_]]=Eval[S,X]})#Ev, ({type St[X]=Store[A,X]})#St]) {

  val ly = new Lens.Yoneda[A, S]

  val Store(g, s) = ly.toHObject(vL)
}