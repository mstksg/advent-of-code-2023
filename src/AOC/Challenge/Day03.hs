{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Set.NonEmpty              as NES
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

day03a :: _ :~> _
day03a = MkSol
    { sParse = Just . first M.keysSet . M.mapEither id . parseAsciiMap (\case
          '.' -> Nothing
          c   -> Just $ maybeToEither c (digitToIntSafe c)
        )
    , sShow  = show
    , sSolve = \(symbolPoints, numPoints) ->
        let numChunks = contiguousRegions (M.keysSet numPoints)
            adjacentToSymbols = S.fromList $
              fullNeighbs =<< S.toList symbolPoints
            validNumChunks = flip S.filter numChunks \numChunk ->
              not $ NES.toSet numChunk `S.disjoint` adjacentToSymbols
            numStrings = map intToDigit . M.elems
                  . M.restrictKeys numPoints
                  . NES.toSet
                <$> S.toList validNumChunks
         in sum <$> traverse (readMaybe @Int) numStrings
    }

day03b :: _ :~> _
day03b = MkSol
    { sParse = Just . first M.keysSet . M.mapEither id . parseAsciiMap (\case
          '.' -> Nothing
          '*' -> Just $ Left ()
          c   -> Right <$> digitToIntSafe c
        )
    , sShow  = show
    , sSolve = \(symbolPoints, numPoints) ->
        let numChunks = contiguousRegions (M.keysSet numPoints)
            adjacentToSymbols = M.fromList do
              p <- S.toList symbolPoints
              n <- fullNeighbs p
              pure (n, p)
            validNumChunks = M.fromListWith (<>) do
              rg <- NES.toSet <$> S.toList numChunks
              let rgDigits = M.elems $ M.restrictKeys numPoints rg
              orig <- M.elems $
                adjacentToSymbols `M.restrictKeys` rg
              pure (orig, S.singleton rgDigits)
         in fmap (sum . map (uncurry (*)) . mapMaybe listTup)
              . traverse (traverse (readMaybe @Int . map intToDigit))
              $ S.toList <$> M.elems validNumChunks
    }
