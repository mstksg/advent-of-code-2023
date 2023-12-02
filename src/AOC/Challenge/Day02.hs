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
import Linear.V3 (V3(..))
import Text.Read (readMaybe)

parseLine :: String -> Maybe (Int, [V3 Int])
parseLine fullLine = do
    (gameNum, specs) <- listTup $ splitOn ": " fullLine
    ("Game", n) <- listTup $ words gameNum
    i <- readMaybe n
    sets <- traverse (setToV3 . splitOn ", ") (splitOn "; " specs)
    pure (i, sets)

-- ["1 red", "2 green", "6 blue"]
setToV3 :: [String] -> Maybe (V3 Int)
setToV3 = fmap pairUp
        . traverse (listTup . reverse . words)
  where 
    pairUp :: [(String, String)] -> V3 Int
    pairUp pairs =
        V3 "red" "green" "blue" <&> \col -> fromMaybe 0 do
          num <- lookup col pairs
          readMaybe num

day02a :: [(Int, [V3 Int])] :~> Int
day02a = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . sum . mapMaybe (\(a,b) -> a <$ guard (all isLegal b))
    }
  where
    isLegal colorVec = and do
      allowed <- V3 12 13 14
      amount <- colorVec
      pure (amount <= allowed)

day02b :: [[V3 Int]] :~> Int
day02b = MkSol
    { sParse = fmap (map snd) . traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . sum . map calcPower
    }
  where
    calcPower = product . foldr (liftA2 max) 0
