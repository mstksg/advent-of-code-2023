-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day10
  ( day10a,
    day10b,
  )
where

import AOC.Solver ((:~>)(..), noFail)
import AOC.Common.Point (Dir(..), Point, dirPoint', inBoundingBox, boundingBox', parseAsciiMap, allDirSet)
import Data.Tuple (swap)
import Control.Monad (guard, join)
import Data.Maybe (listToMaybe, isJust, fromMaybe)
import Data.Foldable (for_)
import Data.List (foldl', unfoldr)
import Data.Map (Map)
import Safe (minimumMay)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES

prepareMap :: Map Point (Maybe (Set Dir)) -> Maybe (Point, Map Point (Set Dir))
prepareMap mp = do
  startPos <- lookup Nothing (swap <$> M.toList mp)
  let dirs = flip S.filter (NES.toSet allDirSet) \d ->
        case join $ M.lookup (startPos + dirPoint' d) mp of
          Nothing -> False
          Just ds -> (d <> South) `S.member` ds
  pure (startPos, fromMaybe dirs <$> mp)

-- | the Dir is the outbound dir
followPath :: Map Point (Set Dir) -> Point -> [(Dir, Point)]
followPath mp startPoint = unfoldr go (startPoint, Nothing)
  where
    go (currPos, lastPos)
      | currPos == startPoint && isJust lastPos = Nothing
      | otherwise = listToMaybe do
          d <- foldMap S.toList $ M.lookup currPos mp
          let nextPos = dirPoint' d + currPos
          for_ lastPos \p -> guard (nextPos /= p)
          pure ((d, currPos), (nextPos, Just currPos))

day10a :: _ :~> _
day10a =
  MkSol
    { sParse = noFail $ parseAsciiMap parseChar,
      sShow = show,
      sSolve = \mp -> do
        (startPos, mp') <- prepareMap mp
        pure $ length (followPath mp' startPos) `div` 2
    }

bothDirs :: [(Dir, a)] -> [(Set Dir, a)]
bothDirs xs = zipWith go xs (drop 1 $ cycle xs)
  where
    go (d', _) (d, p) = (S.fromList [d', d], p)

day10b :: _ :~> _
day10b =
  MkSol
    { sParse = noFail $ parseAsciiMap parseChar,
      sShow = show,
      sSolve = \mp -> do
        (startPos, mp') <- prepareMap mp
        let path = followPath mp' startPos
            loopPoints = S.fromList $ map snd path
        bb <- boundingBox' loopPoints
        let rollUp turner = foldl' go S.empty (bothDirs path)
              where
                go seen (ds, p) = seen <> foldMap lineFrom ds
                  where
                    lineFrom :: Dir -> Set Point
                    lineFrom d =
                      S.fromList
                        . takeWhile (\p' -> inBoundingBox bb p' && p' `S.notMember` loopPoints)
                        . drop 1
                        $ iterate (+ dirPoint' (d <> turner)) p
        minimumMay . map S.size $ [rollUp East, rollUp West]
    }

parseChar :: Char -> Maybe (Maybe (Set Dir))
parseChar = \case
  '|' -> Just $ Just $ S.fromList [North, South]
  '-' -> Just $ Just $ S.fromList [East, West]
  'L' -> Just $ Just $ S.fromList [North, East]
  'J' -> Just $ Just $ S.fromList [North, West]
  '7' -> Just $ Just $ S.fromList [West, South]
  'F' -> Just $ Just $ S.fromList [East, South]
  'S' -> Just Nothing
  _ -> Nothing

_unParse :: Set Dir -> Char
_unParse ds
  | ds == S.fromList [North, South] = '│'
  | ds == S.fromList [East, West] = '─'
  | ds == S.fromList [North, East] = '└'
  | ds == S.fromList [North, West] = '┘'
  | ds == S.fromList [West, South] = '┐'
  | ds == S.fromList [East, South] = '┌'
  | otherwise = '?'
