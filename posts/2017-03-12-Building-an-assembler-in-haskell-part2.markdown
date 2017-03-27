---
title: "Building An Assembler In Haskell : Implementation/Megaparsec"
tags: haskell, emulator
---

# Review
In the last post we built grammar for a simple assembly language, wrote the shell of our parser,
derived some properties from the grammar for a simple parser `byte` and implemented `byte`.

We also saw that there are a few deficiencies in our grammar.

In this post I want to dive deeper into megaparsec, fully implement the language we
spec'd and finally improve it.

# Implementing The Rest Of Our Parsers
The _lexemes_ of a language are the smallest syntactic unit. _Tokens_ are categories of
_lexemes_. In our case, _STORE_ is a lexeme in the category of _label_ tokens. Before we
continue implementing the parsers for our language, let's create convenience functions for
parsing trailing space after our lexemes.

## Lexemes And Space
```{.haskell}
spaceEater :: Parser ()
spaceEater = L.space
  (void spaceChar)
  (L.skipLineComment ";")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceEater
```
`spaceEater` uses _megaparsec's_
[space](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Lexer.html#v:space)
function to build a parser that consumes and discards whitespace and comments. Note that it is
prefixed with `L.` here (`L.space`) because `Text.Megaparsec.Lexer` is imported qualified as
`L`, see [here](https://github.com/wayofthepie/emu-mos6502-asm-blog/blob/e454cce2af3c938e229f1d60a2f3c3d0bf3a3adb/src/Assembler.hs#L6).

Here is what the type for `space` looks like:

```{.haskell}
space :: MonadParsec e s m
      => m () -- ^ A parser for a space character (e.g. @'void' 'C.spaceChar'@)
      -> m () -- ^ A parser for a line comment (e.g. 'skipLineComment')
      -> m () -- ^ A parser for a block comment (e.g. 'skipBlockComment')
      -> m ()
```

The first argument is a parser for space, we use `void spaceChar` here.
[spaceChar](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Char.html#v:spaceChar)
parses a space character, and
[void](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor.html#v:void)
discards the parsed character.

The second argument is a line comment parser. We use a function from
_megaparsec_ called
[skipLineComment](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Lexer.html#v:skipLineComment),
which does what it says - skips line comments starting with the provided character, ";" in
our case.

The last argument is a block comment parser, here we use
[skipBlockComment](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Lexer.html#v:skipBlockComment).
which parses and discards data between "/*" and "*/".

Using `spaceEater` we create a function called `lexeme` which uses [lexeme]() from _megaparsec_ to
build a function that takes a parser and produces a parser which consumes trailing
whitespace and comments, with our `spaceEater`.

I left out the description of the
[MonadParsec](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Prim.html#t:MonadParsec)
typeclass, see
[Jump Into The Parser Types](#jump-into-the-parser-types) for more info.


## bytes
The `bytes` parser parses two bytes, the second being optional. We can use our [single
byte parser](/posts/2017-03-03-Building-an-assembler-in-haskell.html#implementation)
(which we defined in the previous post) to parse each one, along with _megaparsec's_
[option](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec.html#v:option) function
to optionally parse the second byte.

`option x p` tries to apply parser `p`, if `p` fails without consuming input it will return
the value `x`. In our case this is exactly what we want, if `byte` fails to parse when parsing
the second byte we can just return the empty string. As we are building a `Text` value with
this parser we can then append the first byte onto the second and if the second is empty, we
just get the first.

```{.haskell}
bytes :: Parser T.Text
bytes = do
  char '$'
  firstByte <- byte
  anotherByte <- option "" byte
  pure $ T.append firstByte anotherByte
```

## mnemonic
The `mnemonic` parser is just a parser of three upper case characters. Here we use
_megaparsec's_ [count](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec.html#v:count)
and [upperChar](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec.html#v:upperChar)
functions.

  * `count n p` runs the parser `p` `n` times.
  * `upperChar` is a parser for upper case Unicode characters.

Following is the implementation.

```{.haskell}
mnemonic :: Parser Mnemonic
mnemonic = lexeme $ Mnemonic . T.pack <$> mnem
 where
  mnem = count 3 upperChar
```

So what does this actually do?

  * We create a function called `mnem`, defined as `count 3 upperChar`, this
    parses three upper case characters,
  * `T.pack` is `pack` from `Data.Text`, it packs a `String` into a `Text` value.
  * `Mnemonic` is the constructor for our `newtype` which we defined in the last post -
    `newtype Mnemonic = Mnemonic T.Text deriving Show`.
  * `lexeme` we defined above, it eats trailing whitespace and comments.
  * Finally `<$>` is the infix synonym for `fmap`, which lifts a single argument function into
    a
    [Functor](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor.html#t:Functor).


### Putting it all together...
Putting it all together, what we get is a function which parses three upper case characters as a `String`,
we then map `Mnemonic . T.pack` over the
value that `mnem` parses which packs it into a `Text` value and builds a
`Mnemonic` from that value, finally it consumes whitespace or comments after the three characters
with `lexeme`.

## label

```{.haskell}
label :: Parser Label
label = lexeme $ Label . T.pack <$> ((:) <$> letterChar <*>  many alphaNumChar)
```

Lets forget about everything outside of the brackets for now, and only focus on the
following.
```{.haskell}
(:) <$> letterChar <*>  many alphaNumChar
```
To parse a label we first
need to parse _some_ string of any length which starts with a letter, according to our grammar.

[letterChar](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec.html#v:letterChar)
and
[alphaNumChar](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec.html#v:alphaNumChar)
are functions from megaparsec that parse a single letter and a single alpha-numeric
character, respectively.
[many](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec.html#v:many)
parses zero or more occurences of the given parser, so `many alphaNumChar` parses zero or more alpha-numeric
characters.

We saw `<$>` (`fmap`) when building `mnemonic`, however we haven't used
[<*>](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Applicative.html#v:-60--42--62-)
yet.  `<*>` ("apply") is from the
[Applicative](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Applicative.html#t:Applicative)
typeclass, it is just function application for _Applicative Functors_ (see
[Applicative Functors](#applicative-functors) for more info).

### Breaking it down
Now the interesting bit. `letterChar` will parse a single `Char`, and `many alphaNumChar` will parse a `String`, which
is a `[Char]`, we need a way of combining the values produced from these parsers so we just
get a `[Char]`.

To do this we map `:` (the list constructor) over `letterChar` which has
type `Parser Char`. What we end up creating is a function of type
`Parser ([Char] -> [Char])`. Let's assume `letterChar` parses a 'c':

<div class="alert alert-info">
Note that I'm using `p` here as a constructor for values of type `Parser a` for
the intermediate steps. This keeps things
short and simple as the real value of `p` for each step doesn't really matter in this case.
</div>

```{.haskell}
-- In case you forgot, here is the type of (:)
> (:) :: a -> [a] -> [a]

-- So, given
> (:) <$> letterChar <*>  many alphaNumChar

-- After mapping (:) over the value letterChar parsed ('c' in this case) we get the following.
> p ((:) 'c') <*> many alphaNumChar

```

Using `<*>` we then apply `p ((:) 'c')`
over the value of `many alphaNumChar`, which itself has type `Parser [Char]`. Let's assume
`many alphaNumChar` parses "abc123":

```{.haskell}
-- From here
> p ((:) 'c') <*> many alphaNumChar

-- we get
> p ((:) 'c') <*> p "abc123"

-- which gives the following
> p "cabc123"

```

Now we have the value we want, our letter char combined with many alpha-numeric chars,
with the type `Parser [Char]` (or `Parser String`).

### Packing It Up
Following the example above we now have:

```{.haskell}
label = lexeme $ Label . T.pack <$> (p "cabc123")
```
With our parsed string, we pack it into a `Text` value, wrap it up in a `Label`
and parse possible whitespace with `lexeme`. We've seen this above in other
parsers, no need to repeat. So that's `label` implemented!

## labelAssign
Now that `label` is complete, `labelAssign` is simple:

```{.haskell}
labelAssign :: Parser Label
labelAssign = lexeme $ label <* char ':'
```
`<*` is similar to `<*>`, the difference is `<*` discards the value of the second argument.
In our case `label <* char ':'` says parse a label, then parse a ':' but discard it, so it's
not part of the `Label` which `labelAssign` builds.

# Conclusion

# More depth ...

## Jump Into The Parser Types
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

## Functors
`Functor` in haskell is a typeclass. Instances of `Functor` implement the `fmap` function.
It is defined as follows:

```{.haskell}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```
`fmap` takes a function from `a -> b` lifts it into the functor f in the second argument (`f a`)
applying that function to `a`.

## Applicative Functors

[^1]: `Parser` here is a type synonym an as such cannot be a constructor.
