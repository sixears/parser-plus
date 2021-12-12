{- | Utilities for working with parsing, e.g., `Text.Read`, or `Text.Parsec`. -}
module ParserPlus
  ( betweenCs, boundedDoubledChars, braces, brackets, caseInsensitiveChar
  , caseInsensitiveString, commaList, commaSet, convertParser, convertReadParser
  , counts, digits, doubledChar, doubledChars, dQuotedString, many1choice
  , nDecimal, nDecimalDigits, nDecimalDigits', ndigits, ndigitsPadR, nl, parens
  , parse1_2, parse1_2digits, parseBackslashedChar, parseDecimal2_1
  , parseFloat2_1, parseMicros, parseMillis, sepByNE, stringMaybeDQuoted, tries
  , uniquePrefix, utf8BOM, whitespaces

  , tests
  )
where

import Prelude  ( (-), Double, Float, Num, fromIntegral, Int )

-- base --------------------------------

import qualified Data.List.NonEmpty  as  NonEmptyList

import Control.Applicative  ( Alternative, many, optional, pure )
import Control.Monad        ( Monad, liftM2, return, sequence )
import Control.Monad.Fail   ( MonadFail, fail )
import Data.Bifunctor       ( first )
import Data.Char            ( toLower, toUpper )
import Data.Either          ( either )
import Data.Eq              ( Eq )
import Data.Function        ( ($) )
import Data.Functor         ( fmap )
import Data.List            ( dropWhile, filter, foldr1, isPrefixOf, init, inits
                            , last, replicate, tails, zipWith )
import Data.List.NonEmpty   ( NonEmpty( (:|) ), nub )
import Data.Maybe           ( fromMaybe, maybe )
import Data.Ord             ( Ord )
import Data.String          ( String, unlines )
import Data.Foldable        ( foldl1, toList )
import Data.Traversable     ( Traversable )
import Data.Tuple           ( fst )
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Read            ( Read, read, readEither )
import Text.Show            ( show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode          ( (≡) )
import Data.Function.Unicode    ( (∘) )
import Data.Monoid.Unicode      ( (⊕) )
import Numeric.Natural.Unicode  ( ℕ )
import Prelude.Unicode          ( ℤ )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪), (⋫), (∤) )
import Data.MoreUnicode.Char         ( ℂ )
import Data.MoreUnicode.Either       ( 𝔼, pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Maybe        ( 𝕄 )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.String       ( 𝕊 )

-- mtl -----------------------

import Control.Monad.Except  ( MonadError, throwError )

-- natural -----------------------------

import Natural  ( length )

-- nonempty-containers -----------------

import qualified Data.Set.NonEmpty  as  NonEmptySet
import Data.Set.NonEmpty  ( NESet )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNE             ( (⋗), pattern (:⫸) )
import NonEmptyContainers.SeqNEConversions  ( ToSeqNonEmpty( toSeqNE ) )

-- parsec ------------------------------

import Text.Parsec        ( ParseError, SourceName )
import Text.Parsec.Prim   ( Parsec, parse )

-- parsers ------------------------------

import Text.Parser.Combinators  ( Parsing, between, choice, count, eof, option
                                , sepBy1, some, try )
import Text.Parser.Char         ( CharParsing
                                , anyChar, char, digit, noneOf, oneOf )
import Text.Parser.Combinators  ( (<?>), skipOptional, unexpected )

-- tasty -------------------------------

import Test.Tasty  ( TestName, TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertIsLeft, assertLeft, assertRight, runTestsP
                  , runTestsReplay, runTestTree )

--------------------------------------------------------------------------------

{- | `try` the first thing, then the next thing, until the last thing (which
     isn't surrounded by a `try`) -}

tries ∷ (ToSeqNonEmpty ψ, Parsing η, Element ψ ~ η α) ⇒ ψ → η α
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


    testAAB s = testCase s $ 𝕽 s @=? parse aab s s
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

{- | Parse a NonEmpty list of things with a separator; like `sepBy1`, but more
     strongly typed. -}
sepByNE ∷ Alternative γ ⇒ γ α → γ σ → γ (NonEmpty α)
sepByNE x s = NonEmptyList.fromList ⊳ sepBy1 x s

--------------------------------------

{- | Parse a comma-separated non-empty set of values, given a value parser. -}
commaList ∷ ∀ α ρ . (Ord α, CharParsing ρ) ⇒ ρ α → ρ (NonEmpty α)
commaList p = sepByNE p (char ',')

--------------------

commaListTests ∷ TestTree
commaListTests =
  let parse' = parse @𝕊 @_ @(NonEmpty Int)
   in testGroup "commaList"
                [ let (t,e) = ("123,45,6", 123 :| [45, 6])
                   in testCase t $
                        𝕽 e @=? parse' (commaList (read ⊳ some digit)) t t
                ]

----------------------------------------

{- | Parse a comma-separated non-empty set of values, given a value parser.
     A `String` error is thrown if duplicates are detected. -}
commaSet ∷ ∀ α η . (Ord α, MonadError 𝕊 η) ⇒
           SourceName → Parsec 𝕊 () α → 𝕊 → η (NESet α)
commaSet nm p s =
  case parse (commaList p) nm s of
    𝕷 e → throwError (show e)
    𝕽 xs → if xs ≡ nub xs
           then return $ NonEmptySet.fromList xs
           else throwError $ "Duplicates detected in input '" ⊕ s ⊕ "'"

--------------------

commaSetTests ∷ TestTree
commaSetTests =
  testGroup "commaSet"
            [ let t = "1,23,456"
                  e = NonEmptySet.fromList (1 :| [23,456∷ℤ])
               in testCase t $
                    assertRight (e @=?) (commaSet t (read ⊳ some digit) t)
            , let t = "1,23,23"
                  e = "Duplicates detected in input '" ⊕ t ⊕ "'"
               in testCase t $
                    assertLeft (e @=?)
                               (commaSet t (read @Int ⊳ some digit) t)
            ]

----------------------------------------

{- Parse one or two characters, given a character parser. -}
parse1_2 ∷ CharParsing ψ ⇒ ψ ℂ → ψ 𝕊
parse1_2 p = (:) ⊳ p ⊵ (maybe "" pure ⊳ optional p)

----------

parse1_2Tests ∷ TestTree
parse1_2Tests = testGroup "parse1_2" $
  let
    check ∷ 𝕊 → TestTree
    check x = testCase x $
      assertRight (x @=?) $ parse (parse1_2 anyChar ⋪ eof) x x
    checkFail ∷ 𝕊 → 𝕊 → TestTree
    checkFail x e =
      testCase x $ assertLeft (e @=?) $
        first show $ parse (parse1_2 anyChar ⋪ eof) x x
  in
    [ check "x"
    , check "xy"
    , checkFail "" "(line 1, column 1):\nunexpected end of input"
    , checkFail "xyz" (init $ unlines [ "\"xyz\" (line 1, column 3):"
                                      , "unexpected 'z'"
                                      , "expecting end of input"
                                      ])
    ]

--------------------

{- | Parse one or two digits. -}
parse1_2digits ∷ CharParsing ψ ⇒ ψ 𝕊
parse1_2digits = parse1_2 digit

----------

parse1_2digitsTests ∷ TestTree
parse1_2digitsTests = testGroup "parse1_2digits" $
  let
    check ∷ 𝕊 → TestTree
    check x = testCase x $ assertRight (x @=?) $ parse parse1_2digits x x
    checkFail ∷ 𝕊 → 𝕊 → TestTree
    checkFail x e =
      testCase x $ assertLeft (e @=?) $
        first show $ parse parse1_2digits x x
  in
    [ check "1"
    , check "23"
    , checkFail ""
                "(line 1, column 1):\nunexpected end of input\nexpecting digit"
    , checkFail "x" (init $ unlines [ "\"x\" (line 1, column 1):"
                                      , "unexpected \"x\""
                                      , "expecting digit"
                                      ])
    ]

--------------------

{- | Parse a decimal value, with up to 2 digits, with up to two decimal
     places.  Input with no decimal point are allowed, as is input with a
     trailing decimal point.
 -}
parseDecimal2_1 ∷ Read α ⇒ CharParsing ψ ⇒ ψ α
parseDecimal2_1 =
   let
     cat ∷ 𝕄 ℂ → 𝕊
     -- This is for the post-decimal-point bit; if there's nothing (e.g.,
     -- '76.'; we add a 0 to make it '76.0' so that `read` parses correctly.
     -- We do this only for the right-most digit, lest we accidentally insert a
     -- digit ahead of a non-zero digit and thus change value.
     cat a = maybe "0" pure a
     cat' ∷ 𝕊 → 𝕄 𝕊 → 𝕊
     cat' a b = a ⊕ "." ⊕ fromMaybe "0" b
     -- parse a group with leading digits (X, XX, XX., XX.Y)
     p_leading = cat' ⊳ parse1_2digits
                      ⊵ optional (cat ⊳ (char '.' ⋫ optional digit))
     -- parse a group with no leading digits (.Y)
     p_no_lead = (⊕) ⊳ pure "0." ⊵ (char '.' ⋫ (pure ⊳ digit))
   in
     read ⊳ (p_leading ∤ p_no_lead)

----------

parseDecimal2_1Tests ∷ TestTree
parseDecimal2_1Tests = testGroup "parseDecimal2_1" $
  let
    check ∷ 𝕊 → Double → TestTree
    check x d = testCase x $
      assertRight (d @=?) $ parse (parseDecimal2_1 ⋪ eof) x x
    checkFail ∷ 𝕊 → 𝕊 → TestTree
    checkFail x e =
      testCase x $ assertLeft (e @=?) $
        first show $ parse @_ @_ @Double (parseDecimal2_1 ⋪ eof) x x
    unl = init ∘ unlines
  in
    [ check  ".1"    0.1
    , check  "1"     1.0
    , check  "1."    1.0
    , check  "1.0"   1.0
    , check  "1.1"   1.1
    , check "11"    11.0
    , check "11."   11.0
    , check "11.0"  11.0
    , check "11.1"  11.1
    , checkFail "" (unl [ "(line 1, column 1):"
                        , "unexpected end of input"
                        , "expecting digit or \".\""
                        ])
    , checkFail "x" (unl [ "\"x\" (line 1, column 1):"
                         , "unexpected \"x\""
                         , "expecting digit or \".\""
                         ])
    , checkFail "123" (unl [ "\"123\" (line 1, column 3):"
                           , "unexpected '3'"
                           , "expecting \".\" or end of input"
                           ])
    , checkFail "1.23" (unl [ "\"1.23\" (line 1, column 4):"
                            , "unexpected '3'"
                            , "expecting end of input"
                            ])
    ]

--------------------

{- | Parse a Float, one or two digits (pre-decimal), with up to two decimal
     places. -}
parseFloat2_1 ∷ CharParsing ψ ⇒ ψ Float
parseFloat2_1 = parseDecimal2_1

----------

parseFloat2_1Tests ∷ TestTree
parseFloat2_1Tests = testGroup "parseFloat2_1" $
  let
    check ∷ 𝕊 → Float → TestTree
    check x d = testCase x $
      assertRight (d @=?) $ parse (parseFloat2_1 ⋪ eof) x x
    checkFail ∷ 𝕊 → 𝕊 → TestTree
    checkFail x e =
      testCase x $ assertLeft (e @=?) $
        first show $ parse (parseFloat2_1 ⋪ eof) x x
    unl = init ∘ unlines
  in
    [ check  ".1"    0.1
    , check  "1"     1.0
    , check  "1."    1.0
    , check  "1.0"   1.0
    , check  "1.1"   1.1
    , check "11"    11.0
    , check "11."   11.0
    , check "11.0"  11.0
    , check "11.1"  11.1
    , checkFail "" (unl [ "(line 1, column 1):"
                        , "unexpected end of input"
                        , "expecting digit or \".\""
                        ])
    , checkFail "x" (unl [ "\"x\" (line 1, column 1):"
                         , "unexpected \"x\""
                         , "expecting digit or \".\""
                         ])
    , checkFail "123" (unl [ "\"123\" (line 1, column 3):"
                           , "unexpected '3'"
                           , "expecting \".\" or end of input"
                           ])
    , checkFail "1.23" (unl [ "\"1.23\" (line 1, column 4):"
                            , "unexpected '3'"
                            , "expecting end of input"
                            ])
    ]

----------------------------------------

{- | Given a parser (of α), and a checked conversion (from α → β, or a 𝕊 error);
     build a parser for β. -}
convertParser ∷ ∀ α β η . (Monad η, Parsing η) ⇒
                (α → 𝔼 𝕊 β) → η (𝔼 𝕊 α) → η β
convertParser f p = p ≫ either unexpected (either unexpected return ∘ f)

--------------------

{- | Given a parser (of 𝕊), and a checked conversion (from α → β, where α is an
     instance of `Read`); build a parser for β. -}
convertReadParser ∷ ∀ α β η . (Monad η, CharParsing η, Read α) ⇒
                    (α → 𝔼 𝕊 β) → η 𝕊 → η β
convertReadParser f p = convertParser f (readEither ⊳ p)

----------------------------------------

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

----------------------------------------

{- | A parser `p` repeated between `m` & `n` times.  You almost certainly
     want to follow this with something that p would not match. -}
counts ∷ (Alternative φ, Parsing φ) ⇒ ℕ → ℕ → φ α → φ [α]
counts m n p = case [n,n-1..m] of
                 []     → pure []
                 (x:xs) → tries $ (\ i → count (fromIntegral i) p) ⊳ x:|xs

----------------------------------------


{- | Parse between `m` & `n` consecutive digits, right-padding to `n` digits
     with a '0' char.  E.g., for parsing after a decimal point. -}
ndigits ∷ ∀ α φ . (Num α, Read α, CharParsing φ) ⇒ ℕ → ℕ → φ α
ndigits m n = (\ x -> case x of "" -> 0; _ -> read x) ⊳ (counts m n digit)

{- | Parse between `m` & `n` consecutive digits, right-padding to `n` digits
     with a '0' char.  E.g., for parsing after a decimal point. -}
ndigitsPadR ∷ CharParsing φ ⇒ ℕ → ℕ → φ 𝕊
ndigitsPadR m n =
  (padR n '0' ∘ show @ℕ) ⊳ ndigits m n
  where len s = fromIntegral $ length s
        -- we use integer arithmetic here, rather than natural, so that if
        -- (n - len s) were to be negative; replicate gives an empty list rather
        -- than a type error
        padR i c s = s ⊕ replicate (fromIntegral @_ @Int i - len s) c

----------------------------------------

{- | Parse up to `n` digits after a decimal point; returns the digits.  Pads
     out the digits (on the right) with '0's.  If there is no decimal point,
     a string of `n` '0's is returned.  This is to provide a consistent number
     of post-decimal-point numbers, where the user can specify up to `n` digits.
 -}
nDecimalDigits ∷ CharParsing φ ⇒ ℕ → φ 𝕊
nDecimalDigits n = char '.' ⋫ ndigitsPadR 0 (fromIntegral n)

----------

nDecimalDigitsTests ∷ TestTree
nDecimalDigitsTests = testGroup "nDecimalDigits" $
  let
    check ∷ 𝕊 → 𝕊 → TestTree
    check x d = testCase x $
      assertRight (d @=?) $ parse (nDecimalDigits 3 ⋪ eof) x x
    checkFail ∷ 𝕊 → 𝕊 → TestTree
    checkFail x e =
      testCase x $ assertLeft (e @=?) $
        first show $ parse (nDecimalDigits 3 ⋪ eof) x x
    unl = init ∘ unlines
  in
    [ check  ".1"    "100"
    , check  "."     "000"
    , check  ".123"  "123"
    , checkFail ".1234" (unl [ "\".1234\" (line 1, column 5):"
                             , "unexpected '4'"
                             , "expecting end of input"
                             ])
    , checkFail "" (unl [ "(line 1, column 1):"
                        , "unexpected end of input"
                        , "expecting \".\""
                        ])
    ]

----------------------------------------

{- | Like `nDecimalDigits`; but will accept an empty string (no leading '.'),
     which will be "parsed" as a string of '0's.
 -}
nDecimalDigits' ∷ CharParsing φ ⇒ ℕ → φ 𝕊
nDecimalDigits' n =
  fromMaybe (replicate (fromIntegral n) '0') ⊳ optional (nDecimalDigits n)

----------

nDecimalDigits'Tests ∷ TestTree
nDecimalDigits'Tests = testGroup "nDecimalDigits'" $
  let
    check ∷ 𝕊 → 𝕊 → TestTree
    check x d = testCase x $
      assertRight (d @=?) $ parse (nDecimalDigits' 3 ⋪ eof) x x
    checkFail ∷ 𝕊 → 𝕊 → TestTree
    checkFail x e =
      testCase x $ assertLeft (e @=?) $
        first show $ parse (nDecimalDigits' 3 ⋪ eof) x x
    unl = init ∘ unlines
  in
    [ check  ".1"    "100"
    , check  "."     "000"
    , check  ""      "000"
    , check  ".123"  "123"
    , checkFail ".1234" (unl [ "\".1234\" (line 1, column 5):"
                             , "unexpected '4'"
                             , "expecting end of input"
                             ])
    ]

----------------------------------------

{- | Parse a decimal value, with an optional decimal point and up to `n` digits
     after.  The result is returned as a string, which is effectively the int
     multiplied by 10^n; thus, a string "10.23" is returned as "1023".

     If fewer than `n` digits are supplied after the decimal point, the
     "missing" digits are filled in with zeros; thus `nDecimal 3` when parsing
     "10.2" will return "10200".

     Leading zeroes with in the result will be dropped; hence `nDecimal 3` when
     parsing "0.1" will return "100".

     Note that this will not successfully parse an empty string; but a lone
     decimal point will parse as "0"
 -}
nDecimal ∷ CharParsing φ ⇒ ℕ → φ 𝕊
nDecimal n =
  let zeroes = replicate (fromIntegral n) '0'
      dropWhileInit p xs = dropWhile p (init xs) ⊕ [last xs]
   in dropWhileInit (≡ '0') ⊳ (tries $ ((⊕) ⊳ many digit ⊵ nDecimalDigits n)
                                    :| [ (⊕) ⊳ some digit ⊵ pure zeroes ] )

----------

nDecimalTests ∷ TestTree
nDecimalTests = testGroup "nDecimal" $
  let
    check ∷ 𝕊 → 𝕊 → TestTree
    check x d = testCase x $
      assertRight (d @=?) $ parse (nDecimal 3 ⋪ eof) x x
    checkFail ∷ 𝕊 → 𝕊 → TestTree
    checkFail x e =
      testCase x $ assertLeft (e @=?) $
        first show $ parse (nDecimal 3 ⋪ eof) x x
    unl = init ∘ unlines
  in
    [ check  "0.1"    "100"
    , check  "1"     "1000"
    , check  ".1"     "100"
    , check  "."        "0"
    , check  "0"        "0"
    , check  "1.234" "1234"
    , checkFail  ""         (unl [ "(line 1, column 1):"
                                 , "unexpected end of input"
                                 , "expecting digit or \".\""
                                 ])
    , checkFail "1.2345678" (unl [ "\"1.2345678\" (line 1, column 6):"
                                 , "unexpected '5'"
                                 , "expecting end of input"
                                 ])
    ]

----------------------------------------

{- | Parse a milli- value, given as a decimal number of seconds with up to 3
     digits after an (optional) decimal point.  -}

parseMillis ∷ CharParsing φ ⇒ φ 𝕊
parseMillis = nDecimal 3

----------------------------------------

{- | Parse a micro- value, given as a decimal number of seconds with up to 6
     digits after an (optional) decimal point.  -}

parseMicros ∷ CharParsing φ ⇒ φ 𝕊
parseMicros = nDecimal 6

----------------------------------------

{- | Parse a single char from a string, which must be a backslashed double-quote,
     t (tab), n (newline) or backslash. -}
parseBackslashedChar ∷ CharParsing η ⇒ η ℂ

parseBackslashedChar = char '\\' ⋫ foldr1 (∤) [ char '\\' ⋫ pure '\\'
                                              , char 't'  ⋫ pure '\t'
                                              , char 'n'  ⋫ pure '\n'
                                              , char '"'  ⋫ pure '"'
                                              ]

----------

parseBackslashedCharTests ∷ TestTree
parseBackslashedCharTests =
  let prse           ∷ 𝕊 → 𝔼 ParseError ℂ
      prse s         = parse parseBackslashedChar s s
      check          ∷ TestName → ℂ → 𝕊 → TestTree
      check nm c s   = testCase nm $ assertRight (c @=?) (prse s)
      checkFail      ∷ TestName → 𝕊 → TestTree
      checkFail nm s = testCase nm $ assertIsLeft (prse s)
   in testGroup "parseBackslashedChar"
    [ check "newline" '\n' "\\n"
    , check "tab"     '\t' "\\t"
    , check "\\"      '\\' "\\\\"
    , checkFail "a" "\\a"
    , checkFail "\\t" "\t"
    ]

----------------------------------------

{- | Parse a double-quoted string.  Within that string, actual newlines & tabs
     are forbidden: slashes & double-quotes must be backslashed, and newlines
     are '\n', tabs are '\t'.
 -}
dQuotedString ∷ CharParsing η ⇒ η 𝕊
dQuotedString = char '"' ⋫ inner ⋪ char '"'
                where inner =  many (parseBackslashedChar ∤ noneOf "\"\\\n\t")

--------------------

dQuotedStringTests ∷ TestTree
dQuotedStringTests =
  let prse           ∷ 𝕊 → 𝔼 ParseError 𝕊
      prse s         = parse dQuotedString s s
      check          ∷ TestName → 𝕊 → 𝕊 → TestTree
      check nm c s   = testCase nm $ assertRight (c @=?) (prse s)
      checkFail      ∷ TestName → 𝕊 → TestTree
      checkFail nm s = testCase nm $ assertIsLeft (prse s)
   in testGroup "dQuotedString"
    [ check "newline" "\n"  "\"\\n\""
    , check "tab"     "\t"  "\"\\t\""
    , check "\\"      "\\"  "\"\\\\\""
    , check "bob"     "bob" "\"bob\""
    , check "empty"   ""    "\"\""
    , checkFail "a" "\\a"
    , checkFail "\"a" "\"a"
    , checkFail "a\"" "a\""
    , checkFail "\\t" "\t"
    ]

----------------------------------------

{- | Either a double-quoted string (see `dQuotedString`) or else an unquoted
     string, with no newlines, tabs or spaces (and not starting with a
     double-quote).
-}
stringMaybeDQuoted ∷ CharParsing η ⇒ η 𝕊
stringMaybeDQuoted =
  dQuotedString ∤ ((:) ⊳ noneOf ("\"\n\t ") ⊵ many (noneOf "\n\t "))

--------------------

stringMaybeDQuotedTests ∷ TestTree
stringMaybeDQuotedTests =
  let prse           ∷ 𝕊 → 𝔼 ParseError 𝕊
      prse s         = parse (stringMaybeDQuoted ⋪ eof) s s
      check          ∷ TestName → 𝕊 → 𝕊 → TestTree
      check nm c s   = testCase nm $ assertRight (c @=?) (prse s)
      checkFail      ∷ TestName → 𝕊 → TestTree
      checkFail nm s = testCase nm $ assertIsLeft (prse s)
   in testGroup "stringMaybeDQuoted"
    [ check "newline"     "\n"  "\"\\n\""
    , check "tab"         "\t"  "\"\\t\""
    , check "\\"          "\\"  "\"\\\\\""
    , check "bob"         "bob" "\"bob\""
    , check "\"bob cat\"" "bob cat" "\"bob cat\""
    , checkFail           "bob" "bob cat"
    , check "foo"         "foo" "foo"
    , check "empty"       ""    "\"\""
    , check "\\a"         "\\a" "\\a"
    , checkFail           "\"a" "\"a"
    , check "a\""         "a\"" "a\""
    , checkFail           "\\t" "\t"
    ]

------------------------------------------------------------
--                         tests                          --
------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "ParserPlus"
                  [ choicesTests, many1choiceTests, commaListTests
                  , commaSetTests, parse1_2Tests, parse1_2digitsTests
                  , parseDecimal2_1Tests, parseFloat2_1Tests
                  , nDecimalDigitsTests, nDecimalDigits'Tests
                  , nDecimalTests, parseBackslashedCharTests, dQuotedStringTests
                  , stringMaybeDQuotedTests
                  ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
