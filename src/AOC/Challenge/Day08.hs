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

stateMachine :: [Bool] -> [(String, String, String)] -> Map String (Seq (String, Int))
stateMachine lrs xs =
  M.fromList
    [ (a, first (\dir -> if dir then c else b) <$> dirMap)
      | (a, b, c) <- xs
    ]
  where
    iLrs = zip lrs [0 ..]
    dirMap :: Seq (Bool, Int)
    dirMap =
      Seq.fromList $
        zip
          (fst <$> iLrs)
          (drop 1 (cycle (snd <$> iLrs)))

lengthToCond ::
  (String -> Bool) ->
  Map String (Seq (String, Int)) ->
  Map String Int
lengthToCond cond mp = (`Seq.index` 0) <$> res
  where
    res = flip (fmap . fmap) mp \(str, b) ->
      if cond str
        then 1
        else 1 + (res M.! str) `Seq.index` b

day08a :: ([Bool], [(String,String,String)]) :~> Int
day08a =
  MkSol
    { sParse = parseMe . lines,
      sShow = show,
      sSolve = noFail $ \(xs, mp) ->
        let sm = stateMachine xs mp
         in lengthToCond (== "ZZZ") sm M.! "AAA"
    }

day08b :: ([Bool], [(String,String,String)]) :~> Int
day08b =
  MkSol
    { sParse = parseMe . lines,
      sShow = show,
      sSolve = noFail $ \(xs, mp) ->
        let sm = stateMachine xs mp
         in foldr lcm 1
              . M.filterWithKey (\k _ -> last k == 'A')
              $ lengthToCond (\k -> last k == 'Z') sm
    }
