package org.drmathochist

package object cat {
  type ~>[F[_], G[_]] = Natural[F, G]

  type ≈>[Phi[F[_]], Psi[F[_]]] = HNatural[Phi, Psi]
}
