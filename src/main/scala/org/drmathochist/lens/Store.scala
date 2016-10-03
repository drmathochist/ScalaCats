package org.drmathochist.lens

import org.drmathochist.cat.functor.{Comonad, Functor}

object Store {

  implicit def storeIsFunctor[A] = new StoreIsFunctor[A] {}
  trait StoreIsFunctor[A] extends Functor[({type St[X]=Store[A, X]})#St] {
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

case class Store[A, S](get: A, set: A => S)
