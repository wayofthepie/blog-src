---
title: Building An Assembler In Haskell
tags:
---

# MOS Tech 6502
Recently I started building an emulator for the MosTech 6502 Cpu, this post is
about building an assembler for a simple assembly language that compiles to runnable
6502 machine code.

# The Language
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
<hex-value>   := 2 * ([A-Fa-f0-9])
```
The above is a variation of EBNF (Extended Backus Naur Form) notation, we allow regex
(denoted by brackets `()` e.g. `([A-Z])` denotes a single upper case letter, see `<label>`
above) for simplicity.

Here's a breakdown, from the bottom up:

  * `<hex-value>` is defined as `2 * ([A-Fa-f0-9])`, this means two consecutive characters that
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

# Quick And Dirty Lexical Analysis
A direct translation of our grammar into haskell might look something like the following:

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


