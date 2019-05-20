package org.drmathochist.cat

package object hfunctor {
  /**
   * Syntactic sugar that allows us to write Phi ≈> Psi for the type HNatural[Phi, Psi]
   *
   * @tparam Phi The source higher functor of a higher natural transformation
   * @tparam Psi The target higher functor of a higher natural transformation
   */
  type ≈>[Phi[F[_]], Psi[F[_]]] = HNatural[Phi, Psi]

  /**
   * The type HConstant[A, Phi] is always just A.
   *
   * This looks almost identical to Constant, but it's important that the second
   * slot takes a type CONSTRUCTOR, not just a type.  The kind is different!
   */

  type HConstant[A, _[_]] = A

  /**
   * Given a type constructor F, the type Eval[A, F] is just F[A].
   */
  type Eval[A, F[_]] = F[A]
}
