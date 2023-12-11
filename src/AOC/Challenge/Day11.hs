-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
module AOC.Challenge.Day11
  ( day11a,
    day11b,
  )
where

import AOC.Common.Point (Point, boundingBox, mannDist, parseAsciiSet)
import AOC.Solver (dyno_, noFail, (:~>) (..))
import AOC.Util.DynoMap (DynoMap)
import Control.Applicative (liftA2)
import Data.Foldable (toList)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (tails)
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import Linear.V2 (V2 (..))

expandBy :: Int -> NESet Point -> NESet Point
expandBy toAdd orig = NES.mapMonotonic reshape orig
  where
    reshape = flip liftA2 blanks \blankSet x ->
      let underX = IS.size $ IS.takeWhileAntitone (< x) blankSet
       in x + underX * toAdd
    blanks :: V2 IntSet
    blanks = go <$> sequenceA (boundingBox orig) <*> sequenceA (toList orig)
      where
        go (V2 mn mx) xs = IS.fromDistinctAscList [mn .. mx] IS.\\ IS.fromList xs

day11 :: ((?dyno :: DynoMap) => Int) -> NESet Point :~> Int
day11 toAdd =
  MkSol
    { sParse = NES.nonEmptySet . parseAsciiSet (== '#'),
      sShow = show,
      sSolve = noFail $
        \xs ->
          sum
            [ mannDist x y
              | x : ys <- tails $ toList (expandBy toAdd xs),
                y <- ys
            ]
    }

day11a :: NESet Point :~> Int
day11a = day11 1

day11b :: NESet Point :~> Int
day11b = day11 $ dyno_ "expansion" 999999
