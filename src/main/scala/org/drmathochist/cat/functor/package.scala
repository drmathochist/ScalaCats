package org.drmathochist.cat

package object functor {
  type ~>[F[_], G[_]] = Natural[F, G]

  type Constant[A, _] = A
}
