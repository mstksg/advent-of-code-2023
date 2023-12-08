{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
module AOC.Challenge.Day08
  ( day08a,
    day08b,
  )
where

import AOC.Common (LCM (..))
import AOC.Solver (noFail, (:~>) (..))
import Data.Char (isAlphaNum)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

parseMe :: [String] -> Maybe ([Bool], [(String, String, String)])
parseMe = \case
  x : _ : xs ->
    let path = map (== 'R') x
        mp =
          [ (a, filter isAlphaNum b, filter isAlphaNum c)
            | [a, "=", b, c] <- words <$> xs
          ]
     in Just (path, mp)
  _ -> Nothing

stateMachine :: [Bool] -> [(String, String, String)] -> Map String (Seq String)
stateMachine lrs xs =
  M.fromList
    [ (a, (\dir -> if dir then c else b) <$> dirMap)
      | (a, b, c) <- xs
    ]
  where
    dirMap :: Seq Bool
    dirMap = Seq.fromList lrs

pathToCond ::
  (String -> Bool) ->
  Map String (Seq String) ->
  Map String [String]
pathToCond cond mp = (`Seq.index` 0) <$> res
  where
    res = flip (fmap . Seq.mapWithIndex) mp \i str ->
      str : if cond str
        then []
        else (res M.! str) `ixMod` (i + 1)

ixMod :: Seq a -> Int -> a
ixMod xs i = xs `Seq.index` (i `mod` Seq.length xs)

day08a :: ([Bool], [(String, String, String)]) :~> Int
day08a =
  MkSol
    { sParse = parseMe . lines,
      sShow = show,
      sSolve = noFail \(xs, mp) ->
        let sm = stateMachine xs mp
         in length $ pathToCond (== "ZZZ") sm M.! "AAA"
    }

day08b :: ([Bool], [(String, String, String)]) :~> Int
day08b =
  MkSol
    { sParse = parseMe . lines,
      sShow = show,
      sSolve = noFail \(xs, mp) ->
        let sm = stateMachine xs mp
         in getLCM
              . M.foldMapWithKey (\k i -> if last k == 'A' then LCM (length i) else mempty)
              $ pathToCond (\k -> last k == 'Z') sm
    }
