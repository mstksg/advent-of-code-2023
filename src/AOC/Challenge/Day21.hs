{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day21 (
    day21a
  , day21b
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

day21a :: _ :~> _
day21a = MkSol
    { sParse = Just . parseAsciiMap (\case '#' -> Just True; 'S' -> Just False; _ -> Nothing)
    -- , sShow  = ('\n':) . unlines . map (displayAsciiSet '.' 'O')
    , sShow  = show
    , sSolve = noFail $ \mp ->
        let (M.keysSet->pts, head.M.keys->starting) = M.partition id mp
            expand = (`S.difference` pts) . S.fromList . concatMap cardinalNeighbs . S.toList
         in S.size $ iterate expand (S.singleton starting) !!! 64
    }

day21b :: _ :~> _
day21b = MkSol
    { sParse = sParse day21a
    , sShow  = show
    , sSolve = noFail $ \mp ->
        let (M.keysSet->pts, head.M.keys->starting@(V2 x0 y0)) = M.partition id mp
            Just (V2 (V2 xMin yMin) (V2 xMax yMax)) = boundingBox' (M.keys mp)
            maxPoint = V2 (xMax + 1) (yMax + 1)
            expandPoint (V2 bx by, ps) = [
                (V2 (bx + bx') (by + by'), S.singleton (V2 x'' y''))
              | p <- S.toList ps 
              , V2 x' y' <- cardinalNeighbs p
              , let (bx', x'') = x' `divMod` xMax
                    (by', y'') = y' `divMod` yMax
             ]
            expand = M.fromListWith (<>)
                   . concatMap expandPoint
                   . M.toList
         in sum . fmap S.size $
                iterate expand (M.singleton (V2 0 0) (S.singleton starting)) !!! 26501365
    }
