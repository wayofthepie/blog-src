---
title: Building An Assembler In Haskell
tags:
---

# MOS Tech 6502
Recently I started building an emulator for the MosTech 6502 Cpu, this post is
about the initial stages of building an assembler for a simple assembly language
that compiles to runnable 6502 machine code.

# The Language
First, let's define what we want our assembly language to be able to do. To keep it simple,
we only want to allow assignment and the definition of instructions for now:

  * We should be able to define instructions and their operands, e.g. `LDA #$20`. This says
    load the value $20 into the accumulator.
      * `LDA` means load date into the accumulator
      * `#` means this is immediate addressing so use the operand as a _value_ not as an address.
      * `$20` stands for `20` in hexadecimal
  * We should be able to assign values to variables, e.g. `LOCATION = $2020`.
      * Now anywhere we see `LOCATION` we can replace with `$2020`.

Now for a quick and dirty grammar for our simple language:
```
<expr>        := <instruction> | <assignment>
<assignment>  := <label> "=" <bytes>
<instruction> := [<label> ":"] <menmonic> [<label> | <operand>]
<operand>     := ["#"] <bytes>
<bytes>       := "$" <byte> [<byte>]
<label>       := ([A-Za-z]*[A-Za-z0-9]*)
<mnemonic>    := 3 * ([A-Z])
<byte>        := 2 * ([A-Fa-f0-9])
```
The above is a variation of EBNF (Extended Backus Naur Form) notation, we allow regular
expressions (denoted by brackets `()` e.g. `([A-Z])` denotes a single upper case letter)
for simplicity.

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

  * `<instruction>` starts with an optional `<label>` which _must_ be followed by a `:` if
    it exists, this is followed by a `<mnemonic>` and finally an optional `<label>` _xor_ an `<operand>`.
    Some instructions use implicit addressing and require no operand or label, hence why this is
    optional.
    * e.g. `LABEL LDA #$20`
    * Note that the possible label at the end is only for certain instructions, for example
      `BNE LABEL` will jump to the address corresponding to `LABEL` if the zero flag is not
      set.

  * `<assignment>` simply an `=` with a `<label>` on the left hand side and a `<bytes>` on
    the right.
    * e.g. `STORE = $2020`

  * Finally an `<expr>` can be either an `<instruction>` or an `<assignment>`.

An example program would look as follows.

~~~{.no-highlight}
LDA #$01
CMP #$02
BNE notequal
STA $22
notequal: BRK
~~~

And the machine code this compiles to:

~~~{.no-highlight}
a9 01 c9 02 d0 02 85 22 00
~~~

Now that the language is somewhat spec'd out, we have an nice overview of how we can start
building a parser for it. There are a lot of rules not defined in the above spec, for example a
`<label>` cannot match a mnemonic - e.g. `LDA` cannot be a `<label>` - let's not worry about
these for now.

# Building Our Parser
Now that we have our grammar, we can start thinking about how we want to build our
parser. Normally I would work out some types first along with some top-level functions
and go from there, so lets do that.

```{.haskell}
module Assembler where

import qualified Data.Text as T -- from the "text" package
import Text.Megaparsec  -- from the "megaparsec" package

-- | Create a custom parser type. This is megaparsec specific, we will gloss over this in
-- this post.
type Parser = Parsec Dec T.Text

-- | A Label is just a Text value.
newtype Label = Label T.Text deriving Show

-- | Indicates whether an address/ value is preceeded by a "#".
newtype IsImmediate = IsImmediate Bool deriving Show

-- | An address/value of one or two bytes which may have a "#", meaning
-- immediate, before it.
data Operand = Operand IsImmediate T.Text deriving Show

-- | A three letter upper case string.
newtype Mnemonic = Mnemonic T.Text deriving Show

-- | A label, which must be assigned a value.
newtype Var = Var Label  deriving Show

-- | An operand which must be assigned to a label.
newtype Val = Val Operand deriving Show

-- | Either a label or an operand.
data LabelOrOperand = Lbl Label | Op Operand deriving Show

-- | Either an instruction or an assignment.
data Expr
  = Instruction (Maybe Label) Mnemonic (Maybe LabelOrOperand)
  | Assignment Var Val
  deriving Show
```
Most of the values will just be strings (`Text` types) so to distinguish between them we
wrap `Text` in a `newtype` wrapper for each type we care about.
For now we're not going to worry about constructing anything other than strings (I'll be using the
type `Text` to denote strings instead of the built-in `String` in this post [^1]). The _top-level_
functions in this case would be the _symbols_ in our grammar - `<expression>`, `<label>` etc...


```{.haskell}
expression :: Parser Expr
expression = undefined

assignment :: Parser Expr
assignment = undefined

instruction :: Parser Expr
instruction = undefined

operand :: Parser Operand
operand = undefined

bytes :: Parser T.Text
bytes = undefined

labelAssign :: Parser Label
labelAssign = undefined

label :: Parser Label
label = undefined

mnemonic :: Parser Mnemonic
mnemonic = undefined

byte :: Parser T.Text
byte = undefined
```
All functions are `undefined` so the type checker will pass before we
begin to implement the logic. With our grammar, we know what each symbol corresponds
to, so we can use [QuickCheck](https://hackage.haskell.org/package/QuickCheck)
to write properties for each function that adhere to its specification in the
grammar.

## Property Driven Parser Development
To build the parser I'm going to use a parser combinator library called
[megaparsec](https://mrkkrp.github.io/megaparsec/). I
won't go into much detail on megaparsec or parser combinators in this post, simply put,
parser combinators are a way of building more complex parsers by combining parsers.

Looking back at our grammar we have eight symbols, each one can be represented as a
function which is itself a parser for some subset of the grammar. The simplest parser
above would be `byte`, from our grammar this is just a two character hexadecimal string.
Before we start implementing it, let's write a property which encodes what we expect
it to do.

### Generating Our Data - QuickCheck Arbitrary
Using QuickCheck to test parsers is really simple and quite powerful. It involves writing
properties which encode expectations about the ouput of a function given some input.

To build a property for `byte` first we need to create  an
[Arbitrary](https://hackage.haskell.org/package/QuickCheck-2.9.2/docs/Test-QuickCheck-Arbitrary.html#t:Arbitrary)
instance [^2] for
the data it expects - two character hexadecimal strings. Creating an instance of `Arbitrary`
for a type allows random values of that type to be generated, by default
QuickCheck will generate 100 random values of the type each test run. For `byte` this might look
as follows.

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
Here we define a `newtype` called `TwoCharHexString`, which is just a wrapper for `Text`.
Then we create an `Arbitrary` instance for this type which builds two character hex string
`Text` values. Let's run through the instance:

  * [choose](https://hackage.haskell.org/package/QuickCheck-2.9.2/docs/Test-QuickCheck-Gen.html#v:choose)
    generates a random element in the given range, `choose (1, 4)` generates integers between
    1 and 4 inclusive.
  * [elements](https://hackage.haskell.org/package/QuickCheck-2.9.2/docs/Test-QuickCheck-Gen.html#v:elements)
    generates a single value from the given list.
  * With these functions we case generate `x` and `y`, as we do above, and build our two character
    string by building a list of characters made up of `x` and `y` - see `x:[y]` above, this is just a
    `String` - we then pack this with `T.pack` to get our `Text` value.

### Building Our Property
Next we need to write a property that defines what should happen when `byte` parses these
string values.

```{.haskell}
prop_byte_parse (TwoCharHexString s) = parse byte "" s  `shouldParse` s
```
This is simply a function called `prop_byte_parse` which takes a value of type
`TwoCharHexString` runs the parser `byte` with the megaparsec `parse` function [^3] and
checks that the result is as expected, in this case parsing a string `s` should return that
same string. `parse` is a function from the _megaparsec_ package which runs our parser on the
supplied string.

Let's add this to our spec so the property check gets run when we launch `stack test`.

```{.haskell}
asmSpec = do
  describe "byte" $
    it "should parse two consecutive characters in the hex range into a two character string" $
      property prop_byte_parse
```
Running the tests with `stack test` will run this spec and check that the property `prop_byte_parse`
holds under the random values of `TwoCharHexString` that quickcheck produces - which we
ourselves defined in our `Arbitrary` instance.

But wait! The `byte` function was `undefined` so the test should fail! Yip, it should give
output similar to the following..

```
Progress: 1/2
Assembler
  byte
    should parse two consecutive characters in the hex range into a two character string FAILED [1]

Failures:

  test/AssemblerSpec.hs:19:
  1) Assembler.byte should parse two consecutive characters in the hex range into a two character string
       uncaught exception: ErrorCall (Prelude.undefined
       CallStack (from HasCallStack):
         error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
         undefined, called at src/Assembler.hs:52:8 in emu-mos6502-asm-blog-0.1.0.0-J3FxRJn8XZkGwqJZfEo09O:Assembler) (after 1 test)
       *** Failed! (after 1 test):
       Exception:
         Prelude.undefined
         CallStack (from HasCallStack):
           error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
           undefined, called at src/Assembler.hs:52:8 in emu-mos6502-asm-blog-0.1.0.0-J3FxRJn8XZkGwqJZfEo09O:Assembler
       TwoCharHexString "fF"

Randomized with seed 1913513661

Finished in 0.0023 seconds
1 example, 1 failure

```
Good, now we can implement `byte`. I've deliberatly left out some boiler plate such as dependencies
and test setup, but you can view the full code up to this point
[here](https://github.com/wayofthepie/emu-mos6502-asm-blog/tree/ff2c770ec79dc1b56446b80cff28c6bfc87ca57a).


### Implementation
So now that we have a property which defines what our `byte` function should do, we can
implement it. [hexDigitChar](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec.html#v:hexDigitChar)
is a parser from _megaparsec_ which parses a a hexadecimal digit. A byte is made up of two
such digits so `byte` is simply a parser which tries to parse two hexadecimal chars.

```{.haskell}
byte :: Parser T.Text
byte = do
  high <- hexDigitChar
  low <- hexDigitChar
  pure $ T.pack [high,low]
```
Nice! We read a hex char and call it `high` and another called `low` and build a `Text`
value.

The rest of the parsers can be implemented in a similar way, define `Arbitrary` instances
for the data they should take, define properties for the expected output and implement!

I'll leave this implementation for another post. You can check out the code up to this point
[here](https://github.com/wayofthepie/emu-mos6502-asm-blog/tree/90ccef30de0aac6cc0f74df4e688d392c4607846).
Running `stack test` now should give the following output.

```
...
Assembler
  byte
    should parse two consecutive characters in the hex range into a two character string

Finished in 0.0016 seconds
1 example, 0 failures
...
```
Excellent, our implementation passed the property check!

# Conclusion
I'll leave this post here. I think it's long enough! In the next post we'll create the rest
of the parsers and their properties, and also run through _megaparsec_ in some more detail.
There are definitely quite a few improvements that can be added to the language,
and plenty more features that would be useful to have which we can implement in the
future.

There are also some limitations in the grammar, for example it currently does not allow
`X` or `Y` indexed addressing [^4] - e.g. `LDA ($2020,X)` - we can address these too.

This post outlines what I have done so far when building the assembler, and really just
shows my own thought process around designing and implementing. Im actively working on the
6502 emulator, so I hope to do a post every week. The main goal is to outline my development
process in implementing the project, hopefully I'll introduce some bugs or have some
interesting issues along the way!

[^1]: [Haskell String Types](http://www.alexeyshmalko.com/2015/haskell-string-types/) is a
good post detailing the different string types.
[^2]: See the documentation for
[Arbitrary](https://hackage.haskell.org/package/QuickCheck-2.9.2/docs/Test-QuickCheck-Arbitrary.html), this [StackOverflow answer](https://stackoverflow.com/questions/16440208/how-to-generate-arbitrary-instances-of-a-simple-type-for-quickcheck) is also good.
[^3]: See the documentation for [parse](https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec.html#v:parse).
[^4]: See Indexed Indirect, and Indirect Indexed addressing modes
[here](http://www.obelisk.me.uk/6502/addressing.html).
