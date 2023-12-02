-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import AOC.Common (listTup)
import AOC.Solver ((:~>)(..))
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Traversable (for)
import Linear.V3 (V3(..))
import Text.Read (readMaybe)

parseLine :: String -> Maybe (Int, [V3 Int])
parseLine fullLine = do
    [gameNum, specs] <- pure $ splitOn ": " fullLine
    ["Game", n] <- pure $ words gameNum
    i <- readMaybe n
    sets <- for (splitOn "; " specs) $ \chunk -> do
      gs <- traverse (listTup . reverse . words) $ splitOn ", " chunk
      pure $
        V3 "red" "green" "blue" <&> \col ->
          fromMaybe 0 $ readMaybe =<< lookup col gs
    pure (i, sets)

day02a :: [(Int, [V3 Int])] :~> Int
day02a = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . sum . mapMaybe (\(a,b) -> a <$ guard (isLegal b))
    }
  where
    isLegal = all (and . liftA2 (>=) (V3 12 13 14))

day02b :: [[V3 Int]] :~> Int
day02b = MkSol
    { sParse = fmap (map snd) . traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . sum . map calcPower
    }
  where
    calcPower = product . foldr (liftA2 max) 0
