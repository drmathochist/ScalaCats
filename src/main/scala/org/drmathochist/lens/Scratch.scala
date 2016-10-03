package org.drmathochist.lens

import org.drmathochist.cat.hfunctor.{Eval, ≈>, HYoneda}
import org.drmathochist.cat.Coalgebra
import org.drmathochist.cat.functor._
import org.drmathochist.lens.Store.StoreIsFunctor

// following along with https://bartoszmilewski.com/2015/07/13/from-lenses-to-yoneda-embedding/
object Scratch {
  // this says that a Lens is a coalgebra for the functor Store[A, _]
  type Lens[A, S] = Coalgebra[({type St[X]=Store[A, X]})#St, S]

  def firstLens[A, B]: Lens[A, (A, B)] = { case (x, y) => Store(x, xx => (xx, y)) }


  def get[A, X](s: (A, X)) = s._1
  def set[A, X](s: (A, X)): A => (A, X) = (a: A) => s match { case (_, b) => (a, b) }

  def mkLens[A, B](s: (A, B)): Store[A, (A, B)] = Store(get(s), set(s))

  case class IndexedStore[A, B, T](get: A, set: B => T)

  trait IndexedStoreIsIxFunctor extends IxFunctor[IndexedStore] {
    def imap[A, B, S, T](istore: IndexedStore[A, B, S], f: S => T): IndexedStore[A, B, T] = istore match {
      case IndexedStore(x, h) => IndexedStore(x, f compose h)
    }
  }

  trait IndexedStoreIsIxComonad extends IxComonad[IndexedStore] with IndexedStoreIsIxFunctor {
    def iextract[A, T](store: IndexedStore[A, A, T]): T = store match {
      case IndexedStore(a, h) => h(a)
    }
    def iduplicate[A, B, J, T](store: IndexedStore[A, B, T]): IndexedStore[A, J, IndexedStore[J, B, T]] = store match {
      case IndexedStore(a, h) => IndexedStore(a, (j: J) => IndexedStore(j, h))
    }
  }

  implicit object IxStoreDefs extends IndexedStoreIsIxComonad

  def polyset[A, B, X](s: (A, X)): B => (B, X) = (b: B) => s match { case (_, x) => (b, x) }

  def mkixcoalg[A, B, X](s: (A, X)): IndexedStore[A, B, (B, X)] = IndexedStore(get(s), polyset(s))
}
