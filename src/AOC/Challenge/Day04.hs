{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day04 (
    day04a
  , day04b
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

-- Card   1: 72 42 34  7 30  3 25 63 99 15 | 63 30 64 15 72 55 73 32 75 41 37 77 49 51 95 16 25  3 92 18 87  2 71 28 10

day04a :: [(Set Int, Set Int)] :~> Int
day04a = MkSol
    { sParse = traverse (
        listTup <=< traverse (fmap S.fromList . traverse readMaybe . words) . splitOn "|" . (!! 1) . splitOn ":")
        . lines
    , sShow  = show
    , sSolve = noFail $
          sum . map (\(x,y) -> 
            let n = S.size $ x `S.intersection` y
             in if n == 0 then 0 else 2 ^ (n-1)
             )
    }

day04b :: [(Set Int, Set Int)] :~> Int
day04b = MkSol
    { sParse = sParse day04a
    , sShow  = show
    , sSolve = noFail $ \cards ->
        let ixedCards = M.fromList $ zip [1..] cards
         in sum . flip evalState (1 <$ ixedCards) $
              for (M.toList ixedCards) \(i, (a,b)) -> state \currState ->
                let n = S.size $ a `S.intersection` b
                    newCards = M.fromList ((,currState M.! i) . (+i) <$> [1 .. n])
                 in (currState M.! i, M.unionWith (+) newCards currState)
    }
