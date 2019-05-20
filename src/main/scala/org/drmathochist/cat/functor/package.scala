package org.drmathochist.cat

// Some useful functor definitions
package object functor {
  /** Syntactic sugar that allows us to write F ~> G for the type Natural[F, G]
   *
   * @tparam F The source functor of a natural transformation
   * @tparam G The target functor of a natural transformation
   */
  type ~>[F[_], G[_]] = Natural[F, G]

  /**
   * The type Constant[A, X] is always just A.
   *
   * @tparam A The type value to return
   */
  type Constant[A, _] = A
}
