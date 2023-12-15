{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day14
  ( day14a,
    day14b,
  )
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import Data.Group
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

shiftDir :: Set Point -> Dir -> Set Point -> Set Point
shiftDir ps dir rs =
  S.fromList
    [ rotPoint (invert dir) $ V2 x y
      | (x, ys) <- IM.toList fallens,
        y <- IS.toList ys
    ]
  where
    minRow = minimum $ IM.keys =<< IM.elems cols
    cols :: IntMap (IntMap Bool)
    cols =
      IM.fromListWith
        (<>)
        [ (x, IM.singleton y k)
          | (rotPoint dir -> V2 x y, k) <-
              ((,True) <$> S.toList ps)
                ++ ((,False) <$> S.toList rs)
        ]
    mkPiles curr (i, item)
      | item = IM.insert (i + 1) 0 curr
      | otherwise = IM.insert j (count + 1) curr
      where
        (j, count) = IM.findMax curr
    fallens :: IntMap IntSet
    fallens =
      cols <&> \col ->
        let fallen :: IntMap Int
            fallen = foldl' mkPiles (IM.singleton minRow 0) (IM.toList col)
         in IS.fromList
              [ j
                | (i, n) <- IM.toList fallen,
                  j <- [i .. i + n - 1]
              ]

score :: Int -> Set Point -> Int
score maxRow pts = sum $ S.toList pts <&> \(V2 _ y) -> maxRow - y

shiftCycle :: Set Point -> Set Point -> Set Point
shiftCycle ps =
  shiftDir ps East
    . shiftDir ps South
    . shiftDir ps West
    . shiftDir ps North

day14a :: _ :~> _
day14a =
  MkSol
    { sParse = noFail $
        parseAsciiMap $ \case
          'O' -> Just False
          '#' -> Just True
          _ -> Nothing,
      sShow = show,
      sSolve = noFail $ \mp ->
        let (ps, rs) = bimap M.keysSet M.keysSet $ M.partition id mp
            maxRow = maximum (view _y <$> M.keys mp) + 1
         in score maxRow $ shiftDir ps North rs
    }

day14b :: _ :~> _
day14b =
  MkSol
    { sParse = sParse day14a,
      sShow = show,
      sSolve = \mp ->
        let (ps, rs) = bimap M.keysSet M.keysSet $ M.partition id mp
            maxRow = maximum (view _y <$> M.keys mp) + 1
         in fmap (loopMath maxRow ps)
              . findLoopBy id
              . iterate (shiftCycle ps)
              $ rs
    }
  where
    loopMath maxRow ps (V2 (i, x) (j, _)) =
      score maxRow $
        iterate (shiftCycle ps) x !!! leftover
      where
        leftover = (1000000000 - i) `mod` (j - i)
