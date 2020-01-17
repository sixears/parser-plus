{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

{- | Utilities for working with parsing, e.g., `Text.Parser`. -}
module ParserPlus
  ( nl, tries, utf8BOM, whitespaces )
where

-- base --------------------------------

import Control.Applicative  ( many )
import Control.Monad        ( Monad, return )
import Data.Char            ( Char )
import Data.Function        ( ($) )
import Data.String          ( String )
import Data.Foldable        ( foldl1, toList )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋫), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNE             ( (⋗), pattern (:⫸) )
import NonEmptyContainers.SeqNEConversions  ( ToMonoSeqNonEmpty( toSeqNE ) )

-- parsers ------------------------------

import Text.Parser.Combinators  ( Parsing, try )
import Text.Parser.Char         ( CharParsing, char, oneOf )
import Text.Parser.Combinators  ( (<?>), skipOptional )

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

-- that's all, folks! ----------------------------------------------------------
