{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

{- | Utilities for working with parsing, e.g., `Text.Read`, or `Text.Parsec`. -}
module ParserPlus
  ( many1choice, nl, tries, utf8BOM, whitespaces
  , tests )
where

-- base --------------------------------

import Control.Applicative  ( Alternative, many )
import Control.Monad        ( Monad, liftM2, return )
import Data.Char            ( Char )
import Data.Either          ( Either( Right ) )
import Data.Function        ( ($) )
import Data.List            ( init, inits, tails, zipWith )
import Data.String          ( String )
import Data.Foldable        ( foldl1, toList )
import System.Exit          ( ExitCode )
import System.IO            ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode    ( (∘) )
import Data.Monoid.Unicode      ( (⊕) )
import Numeric.Natural.Unicode  ( ℕ )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋪), (⋫), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.String       ( 𝕊 )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNE             ( (⋗), pattern (:⫸) )
import NonEmptyContainers.SeqNEConversions  ( ToMonoSeqNonEmpty( toSeqNE ) )

-- parsec ------------------------------

import Text.Parsec.Prim  ( parse )

-- parsers ------------------------------

import Text.Parser.Combinators  ( Parsing, choice, count, eof, option, try )
import Text.Parser.Char         ( CharParsing, char, oneOf )
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
utf8BOM ∷ CharParsing η ⇒ η Char
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
