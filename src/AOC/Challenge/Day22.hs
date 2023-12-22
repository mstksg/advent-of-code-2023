{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day22
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 22.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day22 (
    day22a
  , day22b
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

type Point3 = V3 Int

dropBrick :: Seq (Set (V3 Int)) -> Seq (Set (V3 Int))
dropBrick Seq.Empty = Seq.Empty
dropBrick (x Seq.:<| xs) = xs Seq.:|> x'
  where
    taken = fold xs
    dropped = S.map (over _z (subtract 1)) x
    x' | S.null (dropped `S.intersection` taken) && all ((> 0) . view _z) dropped = dropped
       | otherwise = x

dropAllOnce :: Seq (Set (V3 Int)) -> Seq (Set (V3 Int))
dropAllOnce xs = strictIterate dropBrick xs !! Seq.length xs

-- firstRepeated :: Ord a => [a] -> Maybe a
-- firstRepeated = firstRepeatedBy id

day22a :: [(V2 (V3 Int))] :~> _
day22a = MkSol
    { sParse = 
          traverse (traverse (traverse readMaybe <=< listV3 . splitOn ",") <=< listV2 . splitOn "~") .lines
    , sShow  = show
    , sSolve = \xs ->
      let bricks = Seq.fromList $ flip map xs \(V2 a b) -> S.fromList $ sequence $ liftA2 enumFromTo a b
       in firstRepeated $ strictIterate dropAllOnce bricks
    }

day22b :: _ :~> _
day22b = MkSol
    { sParse = sParse day22a
    , sShow  = show
    , sSolve = noFail $
          id
    }
