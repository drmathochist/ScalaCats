package org.drmathochist.cat

package object hfunctor {
  type ≈>[Phi[F[_]], Psi[F[_]]] = HNatural[Phi, Psi]

  type HConstant[A, _[_]] = A

  type Eval[A, F[_]] = F[A]
}
