{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day07 (
    day07a
  , day07b
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
import Data.Bitraversable
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

-- 32T3K 765
-- T55J5 684
-- KK677 28
-- KTJJT 220
-- QQQJA 483
-- >>> 6440

day07a :: _ :~> _
day07a = MkSol
    -- { sParse = traverse (_ <=< listTup . words) . lines
    { sParse = traverse (bitraverse pure (readMaybe @Int) <=< listTup . words) . lines
    , sShow  = show
    , sSolve = noFail $
          sum . map (uncurry (*)) . zip [1..] . map snd . sortBy (comparing fst)
          . map (first (\q -> (categorize . map fst . freqList $ q, map qValue q)))
    }
  where
    qValue q = M.findWithDefault undefined q $ M.fromList $
      flip zip [0..] . reverse $ ['A', 'K', 'Q', 'J', 'T'] ++ map intToDigit [9,8.. 2]
    categorize [5] = 7
    categorize [4,1] = 6
    categorize [3,2] = 5
    categorize [3,1,1] = 4
    categorize [2,2,1] = 3
    categorize [2,1,1,1] = 2
    categorize [1,1,1,1,1] = 1
    categorize _ = undefined

day07b :: _ :~> _
day07b = MkSol
    { sParse = traverse (bitraverse pure (readMaybe @Int) <=< listTup . words) . lines
    , sShow  = show
    , sSolve = noFail $
          sum . map (uncurry (*)) . zip [1..] . map snd . sortBy (comparing fst)
          . map (first (\q -> (categorize (map fst . freqList $ filter (/= 'J') q), map qValue q)))
    }
  where
    qValue q = M.findWithDefault undefined q $ M.fromList $
      flip zip [0..] . reverse $ ['A', 'K', 'Q', 'T'] ++ map intToDigit [9,8.. 2] ++ ['J']
    categorize [5] = 7
    categorize [4,1] = 6
    categorize [3,2] = 5
    categorize [3,1,1] = 4
    categorize [2,2,1] = 3
    categorize [2,1,1,1] = 2
    categorize [1,1,1,1,1] = 1
    categorize [4] = 7
    categorize [3,1] = 6
    categorize [2,2] = 5
    categorize [2,1,1] = 4
    categorize [1,1,1,1] = 2
    categorize [3] = 7
    categorize [2,1] = 6
    categorize [1,1,1] = 4
    categorize [2] = 7
    categorize [1,1] = 6
    categorize [1] = 7
    categorize [] = 7
    categorize _ = undefined
