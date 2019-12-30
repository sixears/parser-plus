{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

{- | Utilities for working with parsing, e.g., `Text.Parser`. -}
module ParserPlus
  ( tries )
where

-- base --------------------------------

import Data.Foldable  ( foldl1, toList )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNE             ( (⋗), pattern (:⫸) )
import NonEmptyContainers.SeqNEConversions  ( ToMonoSeqNonEmpty( toSeqNE ) )

-- parsers ------------------------------

import Text.Parser.Combinators  ( Parsing, try )

--------------------------------------------------------------------------------

{- | `try` the first thing, then the next thing, until the last thing (which
     isn't surrounded by a `try`) -}

tries ∷ (ToMonoSeqNonEmpty ψ, Parsing η, Element ψ ~ η α) ⇒ ψ → η α
tries xs = case toSeqNE xs of
             ts :⫸ t → foldl1 (∤) (toList ((try ⊳ ts) ⋗ t))

-- that's all, folks! ----------------------------------------------------------
