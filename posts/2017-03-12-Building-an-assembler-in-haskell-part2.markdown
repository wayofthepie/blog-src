---
title: Building An Assembler In Haskell 2
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

Finally we create a function called `lexeme` which uses [lexeme]() from _megaparsec_ to
build a function that takes a parser and produces a parser which consumes trailing
whitespace and comments, with our `spaceEater`.

## bytes
The `bytes` parser parses two bytes, the second being optional. We can use our single
byte parser `byte` to parse each one, with  _megaparsec's_
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

So, for `mnemonic` we create a function called `mnem` which is `count 3 upperChar`, this
parses three `upperCase` characters.

```{.haskell}
mnemonic :: Parser Mnemonic
mnemonic = lexeme $ Mnemonic . T.pack <$> mnem
 where
  mnem = count 3 upperChar
```

Here we also do a few new things. `T.pack` is `pack` from `Data.Text`, it packs a `String`
into a `Text` value. `Mnemonic` is the constructor for our `newtype` which we defined in the
last post - `newtype Mnemonic = Mnemonic T.Text deriving Show`.


`lexeme` we defined above, it eats trailing whitespace and comments.
Finally `<$>` is the infix synonym for `fmap`, which applies a
function over a
[Functor](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor.html#t:Functor) [^2].

Putting this all together, `lexeme $ Mnemonic . T.pack <$> mnem` gives us a function which
parses three upper case characters as a `String`, we then map `Mnemonic . T.pack` over the
value that `mnem` parses which packs it into a `Text` value and builds a
`Mnemonic` from that value, finally it consumes whitespace after the three characters.


