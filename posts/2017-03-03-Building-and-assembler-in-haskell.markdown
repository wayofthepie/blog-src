---
title: Building An Assembler In Haskell
tags:
---

# MOS Tech 6502
Recently I started building an emulator for the MosTech 6502 Cpu, this post is
about building an assembler for a simple assembly language that compiles to runnable
6502 machine code.

# The Language
<div class="alert alert-warning">Work in progress. Review.</div>

First, let's define what we want our assembly language to be able to do. To keep it simple,
we only want to allow assignment and defining instructions for now:

  * We should be able to define instructions and their operands, e.g. `LDA #$20`. This says
    load the value $20 into the accumulator.
      * `LDA` means load date into the accumulator
      * `#` means this is immediate addressing so use the operand as a _value_ not as an address.
      * `$20` stands for `20` in hexadecimal
  * We should be able to assign values to variables, e.g. `LOCATION = $2020`.
      * Now anywhere we see `LOCATION` we can replace with `$2020`.

Now for a quick and dirty grammar for the language:

```
<expr>        := <instruction> | <assignment>
<assignment>  := <label> "=" <bytes>
<instruction> := [<label>] <menmonic> [<label> | <operand>]
<operand>     := ["#"] <bytes>
<bytes>       := "$" <byte> [<byte>]
<label>       := ([A-Za-z]*[A-Za-z0-9]*)
<mnemonic>    := 3 * ([A-Z])
<byte>   := 2 * ([A-Fa-f0-9])
```
The above is a variation of EBNF (Extended Backus Naur Form) notation, we allow regex
(denoted by brackets `()` e.g. `([A-Z])` denotes a single upper case letter, see `<label>`
above) for simplicity.

Here's a breakdown, from the bottom up:

  * `<byte>` is defined as `2 * ([A-Fa-f0-9])`, this means two consecutive characters that
    are upper or lower case letters betwen `A` and `F` or digits, i.e. a two digit hexadecimal value.
    * e.g. `2F`

  * `<mnemonic>` is a three letter string - all upper case.
    * e.g. `LDA`

  * `<label>` is an alphanumeric string of any size, which must start with a letter.
    * e.g. `Stor3`
    * Note that we _should_ limit it's size, but lets leave it infinite for now!

  * `<bytes>` is a string which starts with a `$` and must contain at least one `<byte>`, at
    most two.
    * e.g. `$2F`

  * `<operand>` starts with an optional `#` followed by `<bytes>`.
    * e.g. `#$2f`

  * `<instruction>` starts with an optional `<label>`, followed by a `<mnemonic>` finally
    followed by an optional `<label>` _xor_ an `<operand>`.
    * e.g. `LABEL LDA #$20`
    * Note that the possible label at the end is only for certain instructions, for example
      `BNE LABEL` will jump to the address corresponding to `LABEL` if the zero flag is not
      set.

  * `<assignment>` simply an `=` with a `<label>` on the left hand side and a `<bytes>` on
    the right.
    * e.g. `STORE = $2020`

  * Finally an `<expr>` can be either an `<instruction>` or an `<assignment>`.

An example program would look like as follows:

~~~{.no-highlight}
LDA #$01
CMP #$02
BNE notequal
STA $22
notequal: BRK
~~~

The machine code this compiles to should look as follows:

~~~{.no-highlight}
a9 01 c9 02 d0 02 85 22 00
~~~

Now that the language is somewhat spec'd out, we have an nice overview of how we can start
building a parser for it. There are a lot of rules not defined in the above spec, for example a
`<label>` cannot match a mnemonic - e.g. `LDA` cannot be a `<label>` - we'll deal with these
later.

# Building Our Parser
Now that we have our grammar, we can start thinking about how we want to build our
assembler. Normally I would work out some types first and go from there, so lets do that.

```{.haskell}
{-# LANGUAGE OverloadedStrings #-}
module Assembler where

import qualified Data.Text as T -- from the "text" package
import Text.Megaparsec hiding (Label, label)

-- | Our custom parser type.
type Parser = Parsec Dec T.Text

newtype Label = Label T.Text deriving Show

newtype IsImmediate = IsImmediate Bool deriving Show

data Operand = Operand IsImmediate T.Text deriving Show

newtype Mnemonic = Mnemonic T.Text deriving Show

newtype Var = Var Label  deriving Show

newtype Val = Val Operand deriving Show

data LabelOrOperand = Lbl Label | Op Operand deriving Show

data Expr
  = Instruction (Maybe Label) Mnemonic (Maybe LabelOrOperand)
  | Assignment Var Val
  deriving Show

```
These correspond to particular sections or our grammar, we compose them just as we did with the
grammar itself, from the bottom up to the expression type `Expr`.

Now that we have an outline of what our types might look like, we can start thinking about
implementing some logic. When building parsers, doing Test Driven Development (TDD) with
QuickCheck has worked out well the past for me, so lets write some tests!

## Property Driven Parser Development (PDPD)
<div class="alert alert-danger">Messy! Review!</div>

To build the parser I'm going to use a parser combinator library called
[megaparsec](https://mrkkrp.github.io/megaparsec/). I
won't go into much detail on megaparsec or parser combinators in this post, simply put
parser combinator libraries give you the ability to build parsers from smaller parsers.

As mentioned you can see all the code on github, [here](), so I will leave out some
boilerplate around setting up the project for tests.

Looking back at our grammar we have eight symbols, each one can be represented as a
top-level function.

```{.haskell}
expression :: Parser
...
byte :: Parser T.Text
byte = undefined
```
The simplest parser above is `byte`, from our grammar this is just a two character
hexadecimal string. Let's write a test for it!

### Testing
Using quickcheck to test parsers is really simple. In this case we want to test the `byte`
parser. First we need to create an `Arbitrary` [^1] instance for two character hexadecimal
strings. This will allow us to randomly generate these strings.

```{.haskell}
-- | Wrapper for our two character hexadecimal strings.
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


```
Now with our strings being generated, we need to write a property that defines what should
happen when `byte` parses them:

```{.haskell}
prop_byte_parse (TwoCharHexString s) = parse byte "" s  `shouldParse` s
```
This is simply a function called `prop_byte_parse` which takes a value of type
`TwoCharHexString` runs the parser `byte` with the megaparsec `parse` function [^2] and
checks that the result is as expected, in this case parsing a string `s` should return that
same string.

Now lets add this to our spec:

```{.haskell}
asmSpec = do
  describe "byte" $
    it "should parse two consecutive characters in the hex range into a two character string" $
      property prop_byte_parse
```
Running the tests with `stack test` will run this spec and check that the `prop_byte_parse`
holds under the random values of `TwoCharHexString` that quickcheck produces.

I've left out necessary dependencies and some boiler plate in this example, but
you can see the full code [here]().

# The Full Parser


```{.haskell}
{-# LANGUAGE OverloadedStrings #-}
module Assembler where

import Control.Monad (void)
import GHC.Word
import qualified Data.Text as T -- from the "text" package
import Text.Megaparsec hiding (Label, label)
import qualified Text.Megaparsec.Lexer as L
import Numeric (readHex)


type Parser = Parsec Dec T.Text

newtype Label = Label T.Text deriving Show

newtype IsImmediate = IsImmediate Bool deriving Show

data Operand = Operand IsImmediate T.Text deriving Show

newtype Mnemonic = Mnemonic T.Text deriving Show

newtype Var = Var Label  deriving Show

newtype Val = Val Operand deriving Show

data LabelOrOperand = Lbl Label | Op Operand deriving Show

data Expr
  = Instruction (Maybe Label) Mnemonic (Maybe LabelOrOperand)
  | Assignment Var Val
  deriving Show

-- | Our top level parser, looking at the grammar this is simply either an instruction or an
assignment.
parseExpression :: Parser Expr
parseExpression = try instruction <|> assignment

-- | Parse an assignment of a label (a variable name in this case, of type 'Var') to a byte
-- value, with immediate or non-immediate addressing.
assignment :: Parser Expr
assignment = do
  labelVal <- label
  parseEquals
  operandVal <- operand
  pure $ Assignment (Var labelVal) (Val operandVal)
 where
  parseEquals = spaceEater *> char '=' *> spaceEater

-- | Parse an instruction.
instruction :: Parser Expr
instruction = lexeme $ Instruction
  <$> (optional $ try labelAssign)
  <*> mnemonic
  <*> optional (Op <$> operand)

-- | Parse an operand, e.g. #$2020.
operand :: Parser Operand
operand = lexeme $ Operand
  <$> (option (IsImmediate False) (char '#' >> (pure $ IsImmediate True)))
  <*> bytes

-- | Parse one or two bytes.
bytes :: Parser T.Text
bytes = lexeme $ do
  char '$'
  byte' <- byte
  anotherByte <- option "" byte
  pure $ T.append byte' anotherByte

-- | Parse the assignment of a label to a location in the program.
labelAssign :: Parser Label
labelAssign = lexeme $  label <* char ':'

-- | Parse a label, recognizes all alpha numeric characters.
label :: Parser Label
label = lexeme $ Label . T.pack <$> ((:) <$> letterChar <*>  many alphaNumChar)

-- | Parse a mnemonic string, recognizes all 3 letter strings.
mnemonic :: Parser Mnemonic
mnemonic = lexeme $ Mnemonic . T.pack <$> op
 where
  op = count 3 letterChar

-- | Parse a two letter hex string.
byte :: Parser T.Text
byte = do
  high <- hexDigitChar
  low <- hexDigitChar
  pure $ T.pack [high,low]

-- | Eats space and comments! Yum!
spaceEater :: Parser ()
spaceEater = L.space
  (void spaceChar)
  (L.skipLineComment ";")
  (L.skipBlockComment "/*" "*/")

-- | A single unit, removes trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceEater
```

[^1]: See the documentation for
[Arbitrary](https://hackage.haskell.org/package/QuickCheck-2.9.2/docs/Test-QuickCheck-Arbitrary.html), this [StackOverflow answer](https://hackage.haskell.org/package/QuickCheck-2.9.2/docs/Test-QuickCheck-Arbitrary.html) is also good.
