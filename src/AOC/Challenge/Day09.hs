-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
module AOC.Challenge.Day09
  ( day09a,
    day09b,
  )
where

import AOC.Solver (noFail, (:~>) (..))
import Control.Monad ((<=<))
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Text.Read (readMaybe)

stepSeries :: NonEmpty Int -> Int
stepSeries = sum . unfoldr go . NE.reverse
  where
    go (x :| xs) = (x,) <$> NE.nonEmpty (zipWith (-) (x : xs) xs)

day09a :: [NonEmpty Int] :~> Int
day09a =
  MkSol
    { sParse =
        traverse (NE.nonEmpty <=< traverse readMaybe . words)
          . lines,
      sShow = show,
      sSolve =
        noFail $
          sum . map stepSeries
    }

day09b :: [NonEmpty Int] :~> Int
day09b =
  MkSol
    { sParse =
        traverse (NE.nonEmpty <=< traverse readMaybe . words)
          . lines,
      sShow = show,
      sSolve =
        noFail $
          sum . map (stepSeries . NE.reverse)
    }
