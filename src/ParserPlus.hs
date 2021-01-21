{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

{- | Utilities for working with parsing, e.g., `Text.Read`, or `Text.Parsec`. -}
module ParserPlus
  ( betweenCs, boundedDoubledChars, braces, brackets, caseInsensitiveChar
  , caseInsensitiveString, digits, doubledChar, doubledChars, many1choice, nl
  , parens, tries, uniquePrefix, utf8BOM, whitespaces

  , tests
  )
where

-- base --------------------------------

import Control.Applicative  ( Alternative, many )
import Control.Monad        ( Monad, liftM2, return, sequence )
import Control.Monad.Fail   ( MonadFail, fail )
import Data.Char            ( toLower, toUpper )
import Data.Either          ( Either( Right ) )
import Data.Eq              ( Eq )
import Data.Function        ( ($) )
import Data.Functor         ( fmap )
import Data.List            ( filter, isPrefixOf, init, inits, tails, zipWith )
import Data.String          ( String )
import Data.Foldable        ( foldl1, toList )
import Data.Traversable     ( Traversable )
import Data.Tuple           ( fst )
import System.Exit          ( ExitCode )
import System.IO            ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode    ( (∘) )
import Data.Monoid.Unicode      ( (⊕) )
import Numeric.Natural.Unicode  ( ℕ )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- more-unicode ------------------------

import Data.MoreUnicode.Char         ( ℂ )
import Data.MoreUnicode.Applicative  ( (⋪), (⋫), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.String       ( 𝕊 )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNE             ( (⋗), pattern (:⫸) )
import NonEmptyContainers.SeqNEConversions  ( ToMonoSeqNonEmpty( toSeqNE ) )

-- parsec ------------------------------

import Text.Parsec.Prim  ( parse )

-- parsers ------------------------------

import Text.Parser.Combinators  ( Parsing, between, choice, count, eof, option
                                , some, try )
import Text.Parser.Char         ( CharParsing, char, digit, noneOf, oneOf )
import Text.Parser.Combinators  ( (<?>), skipOptional )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertIsLeft, runTestsP, runTestsReplay, runTestTree )

--------------------------------------------------------------------------------

{- | `try` the first thing, then the next thing, until the last thing (which
     isn't surrounded by a `try`) -}

tries ∷ (ToMonoSeqNonEmpty ψ, Parsing η, Element ψ ~ η α) ⇒ ψ → η α
tries xs = case toSeqNE xs of
             ts :⫸ t → foldl1 (∤) (toList ((try ⊳ ts) ⋗ t))

----------------------------------------

{- | UTF Byte-Order-Mark, may be seen as the first character of UTF8 files
     https://en.wikipedia.org/wiki/Byte_order_mark -}
utf8BOM ∷ CharParsing η ⇒ η ℂ
utf8BOM = char '\65279'

----------------------------------------

{- | `Text.Parser.Char.spaces` parses *all* spaces, including newline.
     This function skips non-newline whitespaces.
 -}
whitespaces ∷ CharParsing η ⇒ η String
whitespaces = many $ oneOf " \t"

----------------------------------------

{- | Parse a newline, optionally preceded by a carriage-return.  Because of
     windoze. -}
nl ∷ (CharParsing η, Monad η) ⇒ η ()
nl = skipOptional (char '\r') ⋫ char '\n' ⋫ return () <?> "cr/nl"

----------------------------------------

choices ∷ [α] → [(α, [α])]
choices xs = zipWith (\ begin (chosen, end) → (chosen, begin ⊕ end))
                     -- though init is notionally unsafe, tails always produces
                     -- a non-empty list (tails [] ≡ [[]]), so init is safe
                     -- in this instance
                     (inits xs) ((\ (a:as) → (a,as)) ⊳ init (tails xs))

choicesTests ∷ TestTree
choicesTests =
  testGroup "choices"
      [ testCase "123" $ [(1,[2,3]),(2,[1,3]),(3,[1,2])] @=? choices [1,2,3∷ℕ] ]

----------------------------------------

{- | Given a list of parsers, generate a parser that will allow each parser up
     to once in any order.  E.g., many1choice [string "a", string "b"] will
     parse "", "a", "ab", "ba", "b".  We try to be parsimonious about how we do
     that (we don't just generate the cartesian product of possible parses, to
     avoid exponential back-tracking).
  
     Note that you can allow specific repeat counts simply by repeating the
     parser; thus many1choice [string "a", string "a", "string "b"] will parse
     "", "a", "b", "aa", "ab", "ba", "aab", "aba", "baa"
  
     Note also that to avoid backtracking cost, each individual parser is not
     automatically wrapped in a try.  That means that
     many1choice [string "bar", string "baz"]
     will not parse "baz", because the "bar" will consume the "ba" before
     failing.  Thus, if you wish to supply some parsers with common prefices,
     you should wrap those parsers in a try
-}

many1choice ∷ (Monad η, Alternative η) ⇒ [η α] → η [α]
many1choice [] = count 0 $ choice []
many1choice xs = option [] ∘ choice $ many1choice' ⊳ choices xs
-- kept at the top level, rather than in a where, so we get the benefit of
-- memoization
many1choice' ∷ (Monad η, Alternative η) ⇒ (η α, [η α]) → η [α]
many1choice' (x,xs) = liftM2 (:) x (many1choice xs)

many1choiceTests ∷ TestTree
many1choiceTests =
  let
    aab ∷ (Monad η, CharParsing η) ⇒ η 𝕊
    aab = many1choice [char 'a', char 'a', char 'b'] ⋪ eof


    testAAB s = testCase s $ Right s @=? parse aab s s
    testAAB_E s = testCase s $ assertIsLeft (parse aab s s)
   in
    testGroup "many1choice"
              [ testAAB "a"
              , testAAB "b"
              , testAAB "ab"
              , testAAB "ba"
              , testAAB "aa"
              , testAAB "aab"
              , testAAB "aba"
              , testAAB "baa"
              , testAAB "baa"
              , testAAB_E "bb"
              , testAAB_E "bba"
              , testAAB_E "bab"
              , testAAB_E "bba"
              ]

----------------------------------------

{- | Parse between two characters -}
betweenCs ∷ CharParsing η ⇒ ℂ → ℂ → η α → η α
betweenCs l r = between (char l) (char r)

----------------------------------------

{- | Parse between parentheses -}
parens ∷ CharParsing η ⇒ η α → η α
parens = between (char '(') (char ')')

----------------------------------------

{- | Parse between brackets -}
brackets ∷ CharParsing η ⇒ η α → η α
brackets = between (char '[') (char ']')

----------------------------------------

{- | Parse between brackets -}
braces ∷ CharParsing η ⇒ η α → η α
braces = between (char '{') (char '}')

----------------------------------------

{- | Parse 1 or more digits -}
digits ∷ CharParsing η ⇒ η 𝕊
digits = some digit

----------------------------------------

{- | Parse any character except those in `cs`; they must be doubled.  Thus

     @ parse (many (try $ doubledChar "{}")) "test" "o}}{{p}" ≡ Right "o}{p" @

     Note the use of `try`; doubleChar will consume the first char of
     non-conformant input.
 -}
doubledChar ∷ CharParsing η ⇒ [ℂ] → η ℂ
doubledChar cs = (choice $ (\ c → char c ⋫ char c) ⊳ cs) ∤ noneOf cs

----------------------------------------

{- | Parse many characters, most directly, but those in `cs` must be doubled up.

     @ parse (doubledChars "{}") "test" "o}}{{p}x" ≡ Right "o}{p" @
 -}
doubledChars ∷ CharParsing η ⇒ [ℂ] → η 𝕊
doubledChars cs = many (try $ doubledChar cs)

----------------------------------------

{- | Parse many characters, most directly, bounded by `l` on the left and `r`
     on the right; instances of `l` & `r` within the text must be doubled up.

     @ parse (boundedDoubledChars '{' '}') "test" "{o}}{{p}x" ≡ Right "o}{p" @

     @ parse (boundedDoubledChars '!' '!') "test" "!o}}!!p!" ≡ Right "o}}!p" @
 -}
boundedDoubledChars ∷ CharParsing η ⇒ ℂ → ℂ → η 𝕊
boundedDoubledChars l r = betweenCs l r (doubledChars [l,r])

----------------------------------------

{- | Parse a uniquely matching prefix.

     Given a value table, and a parser; can we parse to something that uniquely
     provides a result?  The parser succeeds if the parse output prefixes
     precisely one result.
 -}
uniquePrefix ∷ (MonadFail η, Eq α, Printable χ) ⇒
               [([α],β)] → ([α] → χ) → η [α] → η β
uniquePrefix ss e prs = do
  s ← prs
  case filter ((s `isPrefixOf`) ∘ fst) ss of
    [(_,y)] → return y
    _       → fail $ toString (e s)

----------------------------------------

{- | Parse the given character, or the same character in another case
     (upper or lower). -}
caseInsensitiveChar ∷ (Monad η, CharParsing η) ⇒ ℂ → η ℂ
caseInsensitiveChar c = do
  _ ← char (toLower c) ∤ char (toUpper c)
  return c

--------------------

{- | Parse the given string, but with any combination of upper and lower case
     characters. -}
caseInsensitiveString ∷ (Monad η, CharParsing η, Traversable φ) ⇒ φ ℂ → η (φ ℂ)
caseInsensitiveString = sequence ∘ fmap caseInsensitiveChar

--------------------------------------

{-
eChar ∷ Char
eChar = '\\'

escape :: Parser String
escape = pure ⊳ oneOf "\\\"0nrvtbf{}"

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f{}"

character :: Parser String
character = fmap return nonEscape <|> escape

parseEscaped ∷ String → String → Parser String
parseEscaped l r = do
    strings <- string l *> many character <* string r
    return $ concat strings
-}

------------------------------------------------------------
--                         tests                          --
------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "ParserPlus" [ choicesTests, many1choiceTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
