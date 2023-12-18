{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day18 (
    day18a
  , day18b
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
import Data.Bitraversable
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

-- R 5 (#4f4602)

parseLine :: String -> Maybe (Dir, Int)
parseLine = bitraverse (parseDir . head) readMaybe
        <=< listTup . take 2 . words

day18a :: _ :~> _
day18a = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = noFail $
          \ps -> 
            let 
                qs = fst $ foldl' go (S.empty, 0)  ps
                pathPoints = S.map fst qs
                Just bb = boundingBox' pathPoints
                expandedPoints = foldMap S.fromList $ S.toList qs <&> \(q,d) ->
                  takeWhile (\r -> r `S.notMember` pathPoints && inBoundingBox bb r) . tail $
                    iterate (+ dirPoint' (d <> East)) q
             in S.size $ expandedPoints <> pathPoints
    }
  where
    go (seen, p) (d, i) = (S.fromList ps <> seen, fst $ last ps)
      where
        ps = map (,d) . take (i+1) $ iterate (+ dirPoint' d) p

-- -- | Flood fill from a starting set
-- floodFill
--     :: Ord a
--     => (a -> Set a)     -- ^ Expansion (be sure to limit allowed points)
--     -> Set a            -- ^ Start points
--     -> Set a            -- ^ Flood filled
-- floodFill f = snd . floodFillCount f

day18b :: _ :~> _
day18b = MkSol
    { sParse = sParse day18a
    , sShow  = show
    , sSolve = noFail $
          id
    }
