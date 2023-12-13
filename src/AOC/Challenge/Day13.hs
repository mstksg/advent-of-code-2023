{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day13 (
    day13a
  , day13b
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

-- negate (i-x-0.5) + x+0.5
-- -i+x+0.5+x+0.5
-- -i+x+x+1

findRefl :: Set Point -> IntSet
findRefl pts = IS.fromList findHoriz
            <> IS.fromList ((*100) <$> findVert)
  where
    Just (V2 (V2 xMin yMin) (V2 xMax yMax)) = boundingBox' pts
    cols :: IntMap IntSet
    cols = IM.fromListWith (<>)
      [ (x, IS.singleton y)
      | V2 x y <- toList pts
      ]
    rows :: IntMap IntSet
    rows = IM.fromListWith (<>)
      [ (y, IS.singleton x)
      | V2 x y <- toList pts
      ]
    findHoriz = flip filter [xMin+1 .. xMax] $ \x ->
      let (lt, gt) = IM.spanAntitone (< x) cols
       in and $ zipWith (==) (snd <$> IM.toDescList lt) (snd <$> IM.toAscList gt)
      -- let cols = IntMap IntSet
      --     cols = IM.fromListWith (<>) $ toList pts
      -- let (lt,gt) = S.partition ((< x) . view _x) pts
      --     gt' = S.map (over _x ((+ (x-1)) . negate . subtract x)) gt
      --     inters = lt `S.intersection` gt'
      --  in inters == lt || inters == gt'
    findVert = flip filter [yMin+1 .. yMax] $ \y ->
      let (lt, gt) = IM.spanAntitone (< y) rows
       in and $ zipWith (==) (snd <$> IM.toDescList lt) (snd <$> IM.toAscList gt)
    -- findVert = flip find [yMin+1 .. yMax] $ \y ->
    --   let (lt,gt) = S.partition ((< y) . view _y) pts
    --       gt' = S.map (over _y ((+ (y-1)) . negate . subtract y)) gt
    --       inters = lt `S.intersection` gt'
    --    in inters == lt || inters == gt'
      -- let flipped = S.map (\(V2 i j) -> V2 (-i + 2*x+1) j) pts
      --     -- symDiff = (flipped S.// pts) <> (pts S.// flipped)
      --     -- inters = S.intersection flipped pts
      --  in shiftToZero' (flipped `S.difference` pts) == shiftToZero' (pts `S.difference` flipped)
      -- x <$ guard (S.map (\(V2 i j) -> V2 (negate (i - x)) j) pts == S.map (\(V2 i j) -> V2 (i - x) j) pts)
    -- findVert = flip find [yMin .. yMax] $ \y ->
    --   let flipped = S.map (\(V2 i j) -> V2 i (-j + 2*y+1)) pts
    --       -- symDiff = (flipped S.// pts) <> (pts S.// flipped)
    --       -- inters = S.intersection flipped pts
    --    in shiftToZero' (flipped `S.difference` pts) == shiftToZero' (pts `S.difference` flipped)
    --   -- y <$ guard (S.map (\(V2 i j) -> V2 i (-j + 2*y)) pts == S.map (\(V2 i j) -> V2 i (j - y)) pts)

-- condenseRefl :: Maybe Int -> Maybe Int -> Maybe Int
-- condenseRefl horiz vert = horiz <|> ((* 100) <$> vert)

day13a :: _ :~> _
day13a = MkSol
    { sParse = noFail $ map (parseAsciiSet (== '#')) . splitOn "\n\n"
    , sShow  = show
    -- , sSolve = noFail $ map findRefl
    , sSolve = fmap sum . traverse (fmap fst . IS.minView . findRefl)
    }

findSmudge :: Set Point -> Maybe Int
findSmudge pts = flip firstJust (V2 <$> [xMin .. xMax] <*> [yMin .. yMax]) \pt ->
    let newRefl = findRefl (over (contains pt) not pts)
        isNew = newRefl `IS.difference` origRefl
     in fst <$> IS.minView isNew
     -- in guard (newRefl /= origRefl) *> (fst <$> IS.minView isNew)
    -- mfilter (/= origRefl) $ findRefl (over (contains pt) not pts)
  where
    origRefl = findRefl pts
    Just (V2 (V2 xMin yMin) (V2 xMax yMax)) = boundingBox' pts

day13b :: _ :~> _
day13b = MkSol
    { sParse = sParse day13a
    , sShow  = show
    , sSolve = fmap sum . traverse findSmudge
    }
