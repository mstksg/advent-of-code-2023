{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day24
  ( day24a,
    day24b,
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

-- 213004023520250, 255007063487325, 286351797522218 @ 118, 41, -64

type Point3 = V3 Int

collision ::
  forall f.
  (Show (f (Maybe Rational)), Show (f Int), Num (f Int), Applicative f, Num (f Rational), Foldable f) =>
  V2 (f Int) ->
  V2 (f Int) ->
  Maybe (f Rational)
collision p1@(V2 r1 v1) p2@(V2 r2 v2) = traceShow (p1, p2) do
  Just t' <- allSame t
  pure $ (fromIntegral <$> r1) + t' *^ (fromIntegral <$> v1)
  where
    t :: f (Maybe Rational)
    t = traceShowId $ liftA2 maybeDivide (r2 - r1) (v2 - v1)

-- r_1 + t v_1 = r_2 + t v_2
-- t (v1 - v2) = r2 - r1
--
-- t = (r2x - r1x) / (v1x - v2x)

-- lineIntersect :: forall f. (Show (f (Maybe Rational)), Show (f Int), Num (f Int), Applicative f, Num (f Rational), Foldable f)
--                 => V2 (f Int) -> V2 (f Int) -> Maybe (f Rational)
-- lineIntersect p1@(V2 r1 v1) p2@(V2 r2 v2) = traceShow (p1,p2) do
--   Just t' <- allSame t
--   pure $ (fromIntegral <$> r1) + t' *^ (fromIntegral <$> v1)
--   where
--     t :: f (Maybe Rational)
--     t = traceShowId $ liftA2 maybeDivide (r2 - r1) (v2 - v1)

-- r_1 + t_1 v_1 = r_2 + t_2 v_2
--
-- 2 unknowns, 3 eqs
-- r1x + t1 v1x = r2x + t2 v2x
-- r1y + t1 v1y = r2y + t2 v2y
-- r1z + t1 v1z = r2z + t2 v2z
--
--   v1x t1  -v2x t2 = r2x - r1x
--   v1y t1  -v2y t2 = r2y - r1y
--   v1z t1  -v2z t2 = r2z - r1z
--
--       t1 - v2x/v1x t2 = (r2x - r1x)/v1x
--       t1 - v2y/v1y t2 = (r2y - r1y)/v1y
--       t1 - v2z/v1z t2 = (r2z - r1z)/v1z
--
--       t1 - v2x/v1x t2 = (r2x - r1x)/v1x
--          - v2y/v1y t2 = (r2y - r1y)/v1y - (r2x - r1x)/v1x
--       t1 - v2z/v1z t2 = (r2z - r1z)/v1z
--
-- wait lol in our case it's only two eqs
-- r1x + t1 v1x = r2x + t2 v2x
-- r1y + t1 v1y = r2y + t2 v2y
--
--   v1x t1  -v2x t2 = r2x - r1x
--   v1y t1  -v2y t2 = r2y - r1y
--
--       t1 - v2x/v1x t2 = (r2x - r1x)/v1x
--       t1 - v2y/v1y t2 = (r2y - r1y)/v1y
--
--       t1 - v2x/v1x t2 = (r2x - r1x)/v1x
--          (v2x/v1x - v2y/v1y) t2 = (r2y - r1y)/v1y - (r2x - r1x)/v1x
--          t2 = ((r2y - r1y)/v1y - (r2x - r1x)/v2x)
--                  / (v2x/v1x - v2y/v1y)
--
--       t1 = (r2x - r1x)/v1x + v2x/v1x t2 

intersect2d :: V2 Point -> V2 Point -> Maybe (V2 Rational)
intersect2d p1@(V2 (V2 rx1 ry1) (V2 vx1 vy1)) p2@(V2 (V2 rx2 ry2) (V2 vx2 vy2)) = traceShow (p1,p2) . traceShowId $ do
  a <- maybeDivide (ry2 - ry1) vy1
  b <- maybeDivide (rx2 - rx1) vx1
  c <- maybeDivide vx2 vx1
  d <- maybeDivide vy2 vy1
  guard $ (c - d) /= 0
  let t2 = (a - b) / (c - d)
      t1 = b + t2 * c
  guard $ t2 > 0
  guard $ t1 > 0
  pure $ V2 (fromIntegral rx2 + t2 * fromIntegral vx2) (fromIntegral ry2 + t2 * fromIntegral vy2)

maybeDivide :: Int -> Int -> Maybe Rational
maybeDivide x y
  | y == 0 = Nothing
  | otherwise = Just $ fromIntegral x / fromIntegral y

allSame :: (Eq a, Foldable f) => f a -> Maybe a
allSame xs = case toList xs of
  [] -> Nothing
  y : ys
    | all (== y) ys -> Just y
    | otherwise -> Nothing

day24a :: [V2 (V3 Int)] :~> _
day24a =
  MkSol
    { sParse =
        traverse
          ( traverse (traverse readMaybe <=< listV3 . splitOn ",")
              <=< listV2
              . splitOn "@"
          )
          . lines,
      sShow = show,
      sSolve =
        noFail $ \ps ->
          let inRange =
                all \q ->
                  q >= fromIntegral @Int (dyno_ "mn" 200000000000000)
                    && q <= fromIntegral @Int (dyno_ "mx" 400000000000000)
           in countTrue inRange $
                traceShowId
                  [ p
                    | x : ys <- fmap (fmap (view _xy)) <$> tails ps,
                      y <- ys,
                      p <- maybeToList $ intersect2d x y
                  ]
    }

day24b :: _ :~> _
day24b =
  MkSol
    { sParse = sParse day24a,
      sShow = show,
      sSolve =
        noFail $
          id
    }
