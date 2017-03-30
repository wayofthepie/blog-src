---
title: "Building An Assembler In Haskell : Implementation/Megaparsec"
tags: haskell, emulator
---

# Review
In the last post we wrote a grammar for a simple assembly language, wrote the outline of our parser,
derived some properties from the grammar for a simple parser `byte` and implemented `byte`.
We also saw that there are a few deficiencies in our grammar. In this post I want to
fully implement the language we spec'd, no improvements or changes from the grammar, just
the implementation. For each parser I'll start with some _QuickCheck_ properties the use
those as the spec to implement the parser. Lets get to it!

<hr/>
# Lexemes And Space
The _lexemes_ of a language are the smallest syntactic unit. _Tokens_ are categories of
_lexemes_. In our case, _STORE_ is a lexeme in the category of _label_ tokens. Before we
continue implementing the parsers for our language, let's create convenience functions for
parsing trailing space after our lexemes.

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
    [skipBlockComment](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Lexer.html#v:skipBlockComment).
    which parses and discards data between "/*" and "*/".

Using `spaceEater` we create a function called `lexeme` which uses [lexeme]() from _megaparsec_ to
build a function that takes a parser and produces a parser which consumes trailing
whitespace and comments, with our `spaceEater`.

I left out the description of the
[MonadParsec](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Prim.html#t:MonadParsec)
typeclass, see
[Jump Into The Parser Types](#jump-into-the-parser-types) for more info.

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
prop_byte_parseValidData :: TwoCharHexString -> IO ()
prop_byte_parseValidData (TwoCharHexString s) = parse byte "" s  `shouldParse` s

-- When successful should not consume more input.
prop_byte_parseSuccessShouldNotConsume :: TwoCharHexString -> IO ()
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

Ok, now thats done, we have a spec for our implementation to follow!

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
that if does fail when trying to parse a string that does not start with a letter. So we
need two `Arbitrary` instances - the first, `LabelWithLetter`, is for all letter strings which
start with a letter and the second, `LabelWithNonLetter`, is for strings which start with a
non letter character.

We've seen `shouldParse`, it was used in the `bytes` parser, however we haven't seen
[shouldFailWith](https://hackage.haskell.org/package/hspec-megaparsec-0.3.1/docs/Test-Hspec-Megaparsec.html#v:shouldFailWith)
yet and there seems to be quite a bit to it! Lets break it down.

### shouldFailWith
Sometimes you want to verify that a parser fails on a given input. Not only that, but you
want to verify that the error which is given on that failure contains the right message,
positioin information, etc... `shouldFailWith` allows you to do this. Lets have a look at
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
    using `<>` (this is from `EC`'s
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
and parse possible whitespace with `lexeme`. We've seen this above in other
parsers, no need to repeat.

Phew! There was quite a lot to implementing `label` but we're done now, and can use this
info later in other parsers.

<hr/>
# labelAssign
Now that `label` is complete, `labelAssign` is simple:

```{.haskell}
labelAssign :: Parser Label
labelAssign = lexeme $ label <* char ':'
```
`<*` is similar to `<*>`, the difference is `<*` discards the value of the second argument.
In our case `label <* char ':'` says parse a label, then parse a ':' but discard it, so it's
not part of the `Label` which `labelAssign` builds.

<hr/>
# operand


<hr/>
# Conclusion

<hr/>
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

<hr/>
## Functors
`Functor` in haskell is a typeclass. Instances of `Functor` implement the `fmap` function.
It is defined as follows:

```{.haskell}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```
`fmap` takes a function from `a -> b` lifts it into the functor f in the second argument (`f a`)
applying that function to `a`.

<hr/>
## Applicative Functors

[^1]: `Parser` here is a type synonym an as such cannot be a constructor.
