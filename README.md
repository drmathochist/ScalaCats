This is a toy project I've used to play around with encoding some category-theoretic concepts into Scala, particularly targeting the Yoneda Lemma in two different contexts.

The Yoneda lemma is a very broad and general result, but in Scala I'm mostly concerned with two cases.
The first is functors from types to types, which is what functional programmers usually talk about as "functors".
The second is functors from functors to types.
In this case, the arrows between functors are defined to be natural transformations.
And natural transformations are also the subject matter of the first case of the Yoneda lemma.

As a functional language, Scala already reifies functions as objects, which makes it easy to talk about these arrowe between types.
Natural transformations, however, are trickier.
They show up easily enough: a natural transformation from one functor to another is just a parametrically polymorphic function!
But reifying this object takes a little more work, similar to the way that reifying functions takes work in languages that don't support first-class function values.

The `Yoneda` class encodes a certain bijection between natural transformations and "functor values".
Given a type (say, `String`) we can consider the "representable functor" `String => _` (this is suggestive, but ungrammatical in Scala).
This is a type constructor that, given another type `X`, returns the type of functions `String => X`.
Given another functor (say, `List`) we can consider natural transformations from `String => _` to `List`.
That is, parametrically polymorphic functions that, for any type `X` take a function `String => X` and return a `List[X]`.
Yoneda asserts a bijection between the set of such natural transformations and the set of values of `List[String]`.
We can see evidence of this in the console: 

```console
scala> import org.drmathochist.cat.functor.Yoneda

import org.drmathochist.cat.functor.Yoneda

scala> val y = new Yoneda[String, List]

y: org.drmathochist.cat.functor.Yoneda[String,List] = org.drmathochist.cat.functor.Yoneda@7df4709e

scala> y.toObject(y.toNatural(List("foo", "bar")))

res0: List[String] = List(foo, bar)
```

The higher-order version of Yoneda is similar, but more complicated.
Now, instead of functors from types to types, the source category is that of functors and natural transformations between them.
I encode these concepts as their own types: `HFunctor` and `HNatural`, for "higher" functors and natural transformations, respectively.
Now, given a functor `F`, we have the representable higher functor `F ~> _` of natural transformations from `F` (which is, of course, ungrammatical even after defining the notation `~>`).
Given another higher functor `Phi`, we can consider higher natural transformations from `F ~> _` to `Phi`.
In programming terms, these are parametrically polymorphic functions again, but indexed by type constructors, rather than types.
Now Yoneda asserts a bijection between the set of such higher natural transformations and the set of values of `Phi[F]`.
The class `HYoneda` encodes this bijection.

An important special case arises when `Phi` is an "evaluation" functor.
That is, given a type (say, `Int`), we can define a higher functor `Eval[Int, _]` that takes a functor `F` to the type `F[Int]`.
Because this is a useful case, we define a specialization `EvalHYoneda` of `HYoneda` to make it easier to work with.
Again, we can see evidence of this bijection at work in the console:

```console
scala> import org.drmathochist.cat.hfunctor.EvalHYoneda

import org.drmathochist.cat.hfunctor.EvalHYoneda

scala> val ehy = new EvalHYoneda[List, Int]

ehy: org.drmathochist.cat.hfunctor.EvalHYoneda[List,Int] = org.drmathochist.cat.hfunctor.EvalHYoneda@60e5eed0

scala> ehy.toHObject(ehy.toHNatural(List(1,2,3)))
res1: List[Int] = List(1, 2, 3)

scala> 
```

The `EvalHYoneda` bijection is important in the definition of Lenses in terms of natural transformations.
This approach lays bare the compositionality properties of such lenses.
Bartosz Milewski has some fascinating work exploring this out into the realms of indexed comonads and coalgebras using Haskell.
Reimplementing this work in Scala is still in a scratch state in this project. 

 * https://bartoszmilewski.com/2015/07/13/from-lenses-to-yoneda-embedding/
 