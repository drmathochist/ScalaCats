package org.drmathochist.lens

import org.drmathochist.cat.functor.{Comonad, Functor}

object Store {

  /**
   * For any type A, the type constructor Store[A, _] is a functor
   */
  implicit def storeIsFunctor[A] = new StoreIsFunctor[A] {}
  trait StoreIsFunctor[A] extends Functor[({type St[X]=Store[A, X]})#St] {
    /**
     * The mapped store can still view the same A-valued part, or it can update that part and then apply
     * the given function the overall updated object.
     */
    def fmap[S, T](store: Store[A, S], f: S => T): Store[A, T] = store match {
      case Store(x, h) => Store(x, f compose h)
    }
  }

  implicit def storeIsComonad[A] = new StoreIsComonad[A] {}
  trait StoreIsComonad[A] extends Comonad[({type St[X]=Store[A, X]})#St] with StoreIsFunctor[A] {
    def extract[S](store: Store[A, S]): S = store match {
      case Store(x, h) => h(x)
    }
    def duplicate[S](store: Store[A, S]): Store[A, Store[A, S]] = store match {
      case Store(x, h) => Store(x, (y: A) => Store(y, h))
    }
  }
}

/**
 * A Store is a view of some object of type S, focused on a part of type A.
 *
 * We can "get" the value of the part, or we can "set" a new value, returning an updated object.
 *
 * @tparam A the type of the "part" the store accesses
 * @tparam S the type of the overall structure
 * @param get the getter function to read an A
 * @param set the setter function returning an "updated" S with a new A value
 */
case class Store[A, S](get: A, set: A => S)
