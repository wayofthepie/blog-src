---
title: "Building An Assembler In Haskell : Implementation"
tags: haskell, emulator
---

# Review
In the
[previous post](/posts/2017-03-03-Building-an-assembler-in-haskell.html)
we wrote a grammar for a simple assembly language, wrote the outline of our parser,
derived some properties from the grammar for a simple parser `byte` and implemented `byte`.
We also saw that there are a few deficiencies in our grammar. In this post we'll implement
`bytes`, `menmonic`, `label` and `labelAssign`. For each parser I'll start with some _QuickCheck_
properties then use those as the spec to implement the parser. Let's get to it!

<hr/>
# Imports
First, the imports for modules we use, that should make it clearer where functions are
coming from.

```{.haskell}
import Control.Monad (void)
import qualified Data.Text as T       -- from the "text" package
import Text.Megaparsec hiding (Label, label) -- from the "megaparsec" package
import qualified Text.Megaparsec.Lexer as L -- from the "megaparsec" package
```
Note that `Text.Megaparsec` contains a type `Label` and a function `label`, we use both
these as names for our own type and function for label parsing, so we hide them when
importing.

<hr/>
# Lexemes And Space
The _lexemes_ of a language are the smallest syntactic unit. _Tokens_ are categories of
_lexemes_. In our case, the _"STORE"_ string is an example of a lexeme in the category of
_label_ tokens. Let's assume we can safely eat any whitespace proceeding lexemes.
With this in mind, and before we continue implementing the parsers for our language,
let's create convenience functions for parsing trailing space after our lexemes.

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
Using `spaceEater` we create a function called `lexeme` which we will use to _wrap_ parsers
so they also consume trailing whitespace. This uses
[lexeme](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Lexer.html#v:lexeme)
from _megaparsec_ which takes a space parser, in our case `spaceEater`, and a parser for a
lexeme.

## Megaparsec Space Function
`space` comes from `Text.Megaparsec.Lexer`.
```{.haskell}
space :: MonadParsec e s m
      => m () -- ^ A parser for a space character (e.g. @'void' 'C.spaceChar'@)
      -> m () -- ^ A parser for a line comment (e.g. 'skipLineComment')
      -> m () -- ^ A parser for a block comment (e.g. 'skipBlockComment')
      -> m ()
```

The type of `space` corresponds to the following:

  * The first argument is a parser for space, we use `void spaceChar` here.
    [spaceChar](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Char.html#v:spaceChar)
    parses a space character, and
    [void](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor.html#v:void)
    discards the parsed character.
  * The second argument is a line comment parser. We use a function from
    _megaparsec_ called
    [skipLineComment](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Lexer.html#v:skipLineComment),
    which does what it says - skips line comments starting with the provided character, ";" in
    our case.
  * The last argument is a block comment parser, here we use
    [skipBlockComment](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Lexer.html#v:skipBlockComment),
    which parses and discards data between "/*" and "*/".

I left out the description of the
[MonadParsec](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Prim.html#t:MonadParsec)
typeclass, I'll leave this until a future post where I'll dive into the _megaparsec_ types
in more depth.

<hr/>
# bytes
## Properties
First, let's write some properties!

```{.haskell}
newtype TwoCharHexString = TwoCharHexString T.Text deriving Show

instance Arbitrary TwoCharHexString where
  arbitrary = do
    upper <- choose ('A', 'F')
    lower <- choose ('a', 'f')
    num   <- choose ('0', '9')
    let vals = [upper, lower, num]
    x <- elements vals
    y <- elements vals
    pure $ TwoCharHexString (T.pack (x:[y]))

-- Should parse valid two char hexstring.
prop_byte_parseValidData (TwoCharHexString s) = parse byte "" s  `shouldParse` s

-- When successful should not consume more input.
prop_byte_parseSuccessShouldNotConsume (TwoCharHexString s) extra =
  runParser' byte (initialState (T.append s extra)) `succeedsLeaving` extra
```

`byte` should parse a two character hex string, representing a byte, so we create a newtype
`TwoCharHexString` to represent this. The `Arbitrary` instance is made up of a random
selection of two characters from the set of upper case `A` to `F`, lower case `a` to `f`
letters and any single digit. With this arbitrary instance _QuickCheck_ can now generate
random instances of our `TwoCharHexString` type.

Note the use of:

  * [shouldParse](https://hackage.haskell.org/package/hspec-megaparsec-0.3.1/docs/Test-Hspec-Megaparsec.html#v:shouldParse) -
    `shouldParse` takes a parse result, the expected value and returns an
    [Expectation](https://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/Test-Hspec-Expectations.html#t:Expectation).
    The `Expectation` succeeds if the result is equal to the expected value, and fails otherwise.
  * [succeedsLeaving](https://hackage.haskell.org/package/hspec-megaparsec-0.3.1/docs/Test-Hspec-Megaparsec.html#v:succeedsLeaving) -
    `succeedsLeaving` checks whether a parser has succeeded leaving the specified input untouched.

Simple! We now have a property which checks that the parser parses what we expect and
another which checks that it does not consume more input than it should. As of now I have
nothing in mind for custom error messages, the ones _megaparsec_ spits out are generally
useful enough for parsers this small, so for now I don't think there is a need for a property
which checks the error case.

Ok, now that's done, we have a spec for our implementation to follow!

## Implementation
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
  anotherByte <- option T.empty byte
  pure $ T.append firstByte anotherByte
```
Run the properties and all should be green!

<hr/>
# mnemonic
## Properties
```{.haskell}
newtype ValidMnemonic = ValidMnemonic T.Text deriving Show

instance Arbitrary ValidMnemonic where
  arbitrary = do
    upper  <- choose ('A', 'Z')
    pure $ ValidMnemonic (T.pack [upper, upper, upper])

prop_mnemonic_parseValidMnemString (ValidMnemonic s) =
  parse mnemonic "" s `shouldParse` (Mnemonic s)
```
Really simple, a valid mnemonic is any upper case three letter string, so that's exactly
what our `Arbitrary` instance specifies.

## Implementation
Here we use
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


## Putting it all together...
Putting it all together, what we get is a function which parses three upper case characters as a `String`,
we then map `Mnemonic . T.pack` over the
value that `mnem` parses which packs it into a `Text` value and builds a
`Mnemonic` from that value, finally it consumes whitespace or comments after the three characters
with `lexeme`.

<hr/>
# label
## Properties
```{.haskell}
newtype LabelWithLetter = LabelWithLetter T.Text deriving Show
newtype LabelWithNonLetter = LabelWithNonLetter T.Text deriving Show

instance Arbitrary LabelWithLetter where
  arbitrary = do
    lbl <- genAlphaNum
    lowerLetter <- choose ('a', 'z')
    upperLetter <- choose ('A', 'Z')
    start <- elements [lowerLetter, upperLetter]
    pure . LabelWithLetter $ T.pack (start:lbl)

instance Arbitrary LabelWithNonLetter where
  arbitrary = do
    (LabelWithLetter lbl) <- arbitrary
    nonAlphaChar <- suchThat (arbitrary :: Gen Char) (\s -> not $ isAlpha s)
    pure . LabelWithNonLetter $ T.append (T.pack [nonAlphaChar]) lbl

prop_label_validLabelString (LabelWithLetter lbl) =
  parse label "" lbl `shouldParse` (Label lbl)

prop_label_invalidLabelString (LabelWithNonLetter lbl) =
  parse label "" lbl `shouldFailWith`  err posI (utok (T.head lbl) <> elabel "letter")
```
`label` is a little more involved than the last few parsers. In this case we want to verify
that it fails when trying to parse a string that does not start with a letter. So we
need two `Arbitrary` instances - the first, `LabelWithLetter`, is for all letter strings which
start with a letter and the second, `LabelWithNonLetter`, is for strings which start with a
non letter character.

We've seen `shouldParse`, it was used in the `bytes` parser, however we haven't seen
[shouldFailWith](https://hackage.haskell.org/package/hspec-megaparsec-0.3.1/docs/Test-Hspec-Megaparsec.html#v:shouldFailWith)
yet and there seems to be quite a bit to it! Let's break it down.

### shouldFailWith
Sometimes you want to verify that a parser fails on a given input. Not only that, but you
want to verify that the error which is given on that failure contains the right message,
position information, etc... `shouldFailWith` allows you to do this. Let's have a look at
its type.

```{.haskell}
shouldFailWith :: (Ord t, ShowToken t, ShowErrorComponent e, Show a)
               => Either (ParseError t e) a
               -> ParseError t e
               -> Expectation
```
So it takes:

  * An `Either (ParseError t e) a`, which is the return type of `Text.Megaparsec`'s
    [parse](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec.html#v:parse)
    function.
  * A `ParseError t e`, which we can build with `Test.Hspec.Megaparsec`'s
    [err](https://hackage.haskell.org/package/hspec-megaparsec-0.3.1/docs/Test-Hspec-Megaparsec.html#v:err)
    function.

The `parse` and `err` functions have the following types:

```{.haskell}
err :: NonEmpty SourcePos -- ^ 'ParseError' position
    -> EC t e             -- ^ Error components
    -> ParseError t e     -- ^ Resulting 'ParseError'

parse
  :: Parsec e s a -- ^ Parser to run
  -> String       -- ^ Name of source file
  -> s            -- ^ Input for parser
```

Looking at our `prop_label_invalidLabelString` property:

```{.haskell}
prop_label_invalidLabelString (LabelWithNonLetter lbl) =
  parse label "" lbl `shouldFailWith` err posI (utok (T.head lbl) <> elabel "letter")
```

We run the parser `label` on the string `lbl` which is generated from the `Arbitrary`
instance of `LabelWithNonLetter`. We then assert that it fails with the following
information in the error.

  * The initial character of `lbl` is the cause, this is specified in the `err` call with
    [posI](https://hackage.haskell.org/package/hspec-megaparsec-0.3.1/docs/Test-Hspec-Megaparsec.html#v:posI).
  * It was an unexpected token, `utok (T.head lbl)`.
  * It was expected to be a letter, `elabel letter`.
  * Finally we combine the unexpected token and expected token into one
    [EC](https://hackage.haskell.org/package/hspec-megaparsec-0.3.1/docs/Test-Hspec-Megaparsec.html#t:EC)
    using
    [<>](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Monoid.html#v:-60--62-)
    which is an infix synonymn for
    [mappend](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Monoid.html#v:mappend)
    (this is from `EC`'s
    [Monoid](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Monoid.html#t:Monoid)
    instance) - i.e. `utok (T.head lbl) <> elabel "letter"`.

In English, we expect the `label` parser to fail on any string which does not start with a
letter and the error information should say that it failed at the initial position because
of an unexpected token (giving the actual unexpected character) and finally that it expected
to see a letter here.

## Implementation
```{.haskell}
label :: Parser Label
label = lexeme $ Label . T.pack <$> ((:) <$> letterChar <*>  many alphaNumChar)
```

Let's forget about everything outside of the brackets for now, and only focus on the
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
typeclass, it is just function application for _Applicative Functors_.

### Functor/Applicative Quick Description
A
[Functor](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor.html)
is
something you can map over (a list is a _Functor_) and an 
[Applicative](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Applicative.html)
 (or _Appplicative Functor_) is something
that you can sequence functions through. For a more in depth description see 
[Learn You A Haskell: Functors, Applicative Functors and Monoids](http://learnyouahaskell.com/functors-applicative-functors-and-monoids).
Lists give a nice example comparing the two. Let's
say I have a list of `[1,2]` and want to add `1` to each element. List is a `Functor` so I can
use `fmap`:

```{.haskell}
> fmap (+1) [1,2]
[2,3]

-- Or the inline version
> (+1) <$> [1,2]
[2,3]
```

Now, lets say I have a list `x = [1,2]` and a function _inside_ a list, `[(+1)]`, that I want to
apply to each element of `x`. I can't use `fmap` here as its type is `(a -> b) -> f a -> f b` -
meaning it lifts the function `(a -> b)` into the `Functor` `f a` which gives an `f b`. What we want is a
function, like `fmap`, but where the function to apply is _within_ the `Functor` already -
and this is precisely what `Applicative` gives us with the function `<*>`. The type of `<*>` is:

```{.haskell}
(<*>) :: f (a -> b) -> f a -> f b
```
Excellent! Turns out list also has an `Applicative` instance, so lets use it in our problem:

```{.haskell}
> [(+1)] <*> [1,2]
[2,3]
```
Hopefully that gives some intuition as to what _Functors_ and _Applicative Functors_ are and
how they can be used. It may not be clear yet as to _why_ we use them in our parsers, but I'll
leave that discussion for another post, as it's not really needed for the rest of this post,
and there is quite a bit more to both that I have not mentioned.

## Breaking it down
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

-- After mapping (:) over the value letterChar parsed ('c' in this case) we get
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

-- giving
> p ((:) 'c' "abc123")

-- which gives
> p "cabc123"

```

Now we have the value we want, our letter char combined with many alpha-numeric chars,
with the type `Parser [Char]` (or `Parser String`).

## Packing It Up
Following the example above we now have:

```{.haskell}
label = lexeme $ Label . T.pack <$> (p "cabc123")
```

With our parsed string, we pack it into a `Text` value, wrap it up in a `Label`
and parse (and discard) possible whitespace with `lexeme`. We've seen this above in other
parsers, no need to repeat.

Phew! There was quite a lot to implementing `label` but we're done now, and can use this
info later in other parsers.

<hr/>
# labelAssign
Now that `label` is complete, `labelAssign` is simple. We will build it from `label` so
really when creating a property all we need to check is that it parses and discards a ':'.

```{.haskell}
prop_labelAssign_shouldDiscardColon (LabelWithLetter lbl) =
  parse labelAssign "" (T.snoc lbl ':') `shouldParse` Label lbl
```

The implementation:

```{.haskell}
labelAssign :: Parser Label
labelAssign = lexeme $ label <* char ':'
```
`<*` is similar to `<*>`, the difference is `<*` discards the value of the second argument.
In our case `label <* char ':'` says parse a label, then parse a ':' but discard it, so it's
not part of the `Label` which `labelAssign` builds.

<hr/>
# Conclusion
Now we have ~80% of our parser completed, properties for each of the parsers,
and some understanding (I hope) of one way to test _megaparsec_ parsers.
In the next post I'll dive deeper into _megaparsec_ and  implement the remaining parsers.
You can view the code up to this point at
<https://github.com/wayofthepie/emu-mos6502-asm-blog/tree/hasm-blog02>.
