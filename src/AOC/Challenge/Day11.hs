{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day11
  ( day11a,
    day11b,
  )
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map as M
import qualified Data.OrdPSQ as PSQ
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

expand :: Set Point -> Set Point
expand orig = S.map reshape orig
  where
    reshape (V2 x y) = V2 (x + underX) (y + underY)
      where
        underX = length $ takeWhile (< x) blankXs
        underY = length $ takeWhile (< y) blankYs
    Just (V2 (V2 xMin yMin) (V2 xMax yMax)) = boundingBox' orig
    blankXs = flip filter [xMin .. xMax] \x -> S.null $ orig `S.intersection` (S.fromList $ V2 x <$> [yMin .. yMax])
    blankYs = flip filter [yMin .. yMax] \y -> S.null $ orig `S.intersection` (S.fromList $ flip V2 y <$> [xMin .. xMax])

expand2 :: Set Point -> Set Point
expand2 orig = S.map reshape orig
  where
    reshape (V2 x y) = V2 (x + underX * (1000000 - 1)) (y + underY * (1000000 - 1))
      where
        underX = length $ takeWhile (< x) blankXs
        underY = length $ takeWhile (< y) blankYs
    Just (V2 (V2 xMin yMin) (V2 xMax yMax)) = boundingBox' orig
    blankXs = flip filter [xMin .. xMax] \x -> S.null $ orig `S.intersection` (S.fromList $ V2 x <$> [yMin .. yMax])
    blankYs = flip filter [yMin .. yMax] \y -> S.null $ orig `S.intersection` (S.fromList $ flip V2 y <$> [xMin .. xMax])

day11a :: _ :~> _
day11a =
  MkSol
    { sParse = noFail $ parseAsciiSet (== '#'),
      -- , sShow  = ('\n':) . displayAsciiSet '.' '#'
      sShow = show,
      sSolve = noFail $
        \xs ->
          let xs' = expand xs
           in sum
                [ mannDist x y
                  | (x :: Point, ys) <- select (toList xs'),
                    y :: Point <- ys
                ]
                `div` 2
    }

day11b :: _ :~> _
day11b =
  MkSol
    { sParse = sParse day11a,
      sShow = show,
      sSolve = noFail $
        \xs ->
          let xs' = expand2 xs
           in sum
                [ mannDist x y
                  | (x :: Point, ys) <- select (toList xs'),
                    y :: Point <- ys
                ]
                `div` 2
    }
