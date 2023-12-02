{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day02 (
    day02a
  , day02b
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
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

-- Game 1: 14 green, 8 blue, 9 red; 5 blue, 4 green, 2 red; 4 red, 4 blue, 4 green; 1 blue, 3 green, 2 red; 10 red, 3 blue, 15 green; 2 red, 6 green, 3 blue

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

day02a :: _ :~> _
day02a = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . sum . mapMaybe (\(a,b) -> a <$ guard (isLegal b))
    }
  where
    isLegal = all (and . liftA2 (>=) (V3 12 13 14))

day02b :: _ :~> _
day02b = MkSol
    { sParse = sParse day02a
    , sShow  = show
    , sSolve = Just . sum . map (calcPower . snd)
    }
  where
    calcPower = product . foldr (liftA2 max) 0
