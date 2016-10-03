package org.drmathochist

package object cat {
  type Coalgebra[W[_], S] = S => W[S]

  type IxCoalgebra[W[_, _, _], S, T, A, B] = S => W[A, B, T]
}
