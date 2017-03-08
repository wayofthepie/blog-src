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

1. FIXME : Talk about tokens, lexemes, whitespace, comments etc....
2. Add support for comments
3. Implement with properties for each parser also

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


`lexeme` we defined above. Finally `<$>` is the infix synonym for `fmap`, which applies a
function over a
[Functor](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor.html#t:Functor) [^2].

Putting this all together, `lexeme $ Mnemonic . T.pack <$> mnem` gives us a function which
parses three upper case characters as a `String`, we then map `Mnemonic . T.pack` over the
value mnem parses which packs it into a `Text` value and builds a
`Mnemonic` from that value, finally it consumes whitespace after the three characters.
