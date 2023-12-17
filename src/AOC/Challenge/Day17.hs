{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day17 (
    day17a
  , day17b
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

day17a :: _ :~> _
day17a = MkSol
    { sParse = noFail $ parseAsciiMap digitToIntSafe
    , sShow  = show
    , sSolve = \mp -> do
        V2 p1 p2 <- boundingBox' (M.keys mp)
        let grow (lastThree, p) = M.fromList
              [ ((take 3 (d : lastThree), p'), cost)
                | d <- toList allDir
              , case lastThree of
                  [] -> True
                  dd : _ -> dd /= d <> South
              , lastThree /= replicate 3 d
              , let p' = p + dirPoint d
              , cost <- maybeToList $ M.lookup p' mp
              ]
        fst <$> aStar ((`mannDist` p2) . snd) grow ([],p1) ((== p2) . snd)
    }

-- aStar
--     :: forall n p. (Ord n, Ord p, Num p)
--     => (n -> p)         -- ^ heuristic
--     -> (n -> Map n p)   -- ^ neighborhood
--     -> n                -- ^ start
--     -> (n -> Bool)      -- ^ target
--     -> Maybe (p, [n])   -- ^ the shortest path, if it exists, and its cost

day17b :: _ :~> _
day17b = MkSol
    { sParse = sParse day17a
    , sShow  = show
    , sSolve = \mp -> do
        V2 p1 p2 <- boundingBox' (M.keys mp)
        let grow (lastDir, p) = M.fromList
              [ ((Just d, p'), sum (snd <$> steps))
                | d <- toList allDir
              , case lastDir of
                  Nothing -> True
                  Just d' -> d /= d' && d /= (d' <> South)
              , steps <- drop 4 $ inits do
                  i <- [1..10]
                  let p' = p + i *^ dirPoint d
                  cost <- maybeToList $ M.lookup p' mp
                  pure (p', cost)
              , (p', _) <- maybeToList $ lastMay steps
              ]
        fst <$> aStar ((`mannDist` p2) . snd) grow (Nothing,p1) ((== p2) . snd)
    }
