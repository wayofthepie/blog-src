---
title: "Building An Assembler In Haskell : More Implementation and Megaparsec"
tags: haskell, emulator
---

<hr/>
# Jump Into The Parser Types
The
[MonadParsec](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Prim.html#t:MonadParsec)
typeclass defines some general combinators
(
[try](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Prim.html#v:try),
[lookahead](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Prim.html#v:lookAhead),
etc...
), it's type variables `e`, `s` and `m` correspond to instances of the following:

  * `e` : [ErrorComponent](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Error.html#t:ErrorComponent)
  * `s` : [Stream](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Prim.html#t:Stream)
  * `m` : [MonadPlus]() _and_ [Alternative]()

These instances come from our `Parser` type, which we
[defined](https://github.com/wayofthepie/emu-mos6502-asm-blog/blob/e454cce2af3c938e229f1d60a2f3c3d0bf3a3adb/src/Assembler.hs#L8)
as:

```{.haskell}
type Parser = Parsec Dec T.Text
```
This gives us: `e` is `Dec`, `s` is `Text` and `m` is `Identity`. `Parsec Dec T.Text` (see
[Parsec](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Prim.html#t:Parsec)
) is a type synonym for `ParsecT Dec Text Identity`, and
[ParsecT](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Prim.html#t:ParsecT)
has the type:

```{.haskell}
data ParsecT e s m a
```
So wherever you see the type `Parser a` in our code it is a synonym for
`ParsecT Dec Text Identity a`.

<hr/>
# Functors
`Functor` in haskell is a typeclass. Instances of `Functor` implement the `fmap` function.
It is defined as follows:

```{.haskell}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```
`fmap` takes a function from `a -> b` lifts it into the functor f in the second argument (`f a`)
applying that function to `a`.

<hr/>
# Applicative Functors


