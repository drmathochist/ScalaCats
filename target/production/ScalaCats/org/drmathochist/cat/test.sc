import org.drmathochist.cat.functor.Yoneda
import org.drmathochist.cat.hfunctor.EvalHYoneda
val y = new Yoneda[String, List]



y.toObject(y.toNatural(List("foo", "bar")))

import org.drmathochist.cat.functor.Applicative._
List(1,2,3).ap(List((_: Int)*0, (_: Int)+100, (x: Int) => x*x))
val ehy = new EvalHYoneda[List, Int]



ehy.toHObject(ehy.toHNatural(List(1,2,3)))