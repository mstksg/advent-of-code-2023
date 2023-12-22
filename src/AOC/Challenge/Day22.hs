{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveAnyClass #-}

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

data Axis = X | Y | Z
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NFData

data Block = Block
    { bAxis :: Axis
    , bStart :: Point3
    , bSize :: Int
    }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NFData

classifyBlock :: V2 Point3 -> Maybe Block
classifyBlock (V2 p0@(V3 x y z) p1@(V3 x' y' z')) = case liftA2 compare p0 p1 of
  V3 EQ EQ EQ -> Just $ Block X p0 0
  V3 LT EQ EQ -> Just $ Block X p0 (x' - x)
  V3 GT EQ EQ -> Just $ Block X p1 (x - x')
  V3 EQ LT EQ -> Just $ Block Y p0 (y' - y)
  V3 EQ GT EQ -> Just $ Block Y p1 (y - y')
  V3 EQ EQ LT -> Just $ Block Z p0 (z' - z)
  V3 EQ EQ GT -> Just $ Block Z p1 (z' - z)
  _ -> Nothing

minZ :: Block -> Int
minZ = view _z . bStart

maxZ :: Block -> Int
maxZ Block{..} = case bAxis of
    X -> view _z bStart
    Y -> view _z bStart
    Z -> view _z bStart + bSize

footprint :: Block -> Set Point
footprint Block{..} = case bAxis of
    X -> S.fromAscList [ view _xy bStart + V2 dx 0 | dx <- [0..bSize] ]
    Y -> S.fromAscList [ view _xy bStart + V2 0 dy | dy <- [0..bSize] ]
    Z -> S.singleton (view _xy bStart)

-- | Blocks indixed by max X
type AirColumn = Map Int [Block]

dropBrick :: AirColumn -> Block -> Block
dropBrick ac b@Block{..} = _
  where
    contenders = M.takeWhileAntitone (< minZ b) ac
    firstStop = M.dropWhileAntitone _ contenders
--   -- case bAxis of
--   --   X -> _
--   --   Y -> _
--   --   Z -> _

dropBrick :: Seq (Set (V3 Int)) -> Seq (Set (V3 Int))
dropBrick Seq.Empty = Seq.Empty
dropBrick (x Seq.:<| xs) = xs Seq.:|> x'
  where
    taken = fold xs
    valid q = all ((> 0) . view _z) q
      && S.null (q `S.intersection` taken)
    x' = last . takeWhile valid $ iterate (S.map (over _z (subtract 1))) x

    -- dropped = S.map (over _z (subtract 1)) x
    -- x' | S.null (dropped `S.intersection` taken) && all ((> 0) . view _z) dropped = dropped
    --    | otherwise = x

dropAllOnce :: Seq (Set (V3 Int)) -> Seq (Set (V3 Int))
dropAllOnce xs = strictIterate dropBrick xs !! Seq.length xs

-- firstRepeated :: Ord a => [a] -> Maybe a
-- firstRepeated = firstRepeatedBy id

day22a :: [V2 (V3 Int)] :~> _
day22a = MkSol
    { sParse =
          traverse (traverse (traverse readMaybe <=< listV3 . splitOn ",") <=< listV2 . splitOn "~") .lines
    , sShow  = show
    , sSolve = \xs ->
        traverse classifyBlock xs
      -- let bricks = Seq.fromList $ flip map xs \(V2 a b) -> S.fromList $ sequence $ liftA2 enumFromTo a b
      --  in firstRepeated $ strictIterate dropAllOnce bricks
    }

day22b :: _ :~> _
day22b = MkSol
    { sParse = sParse day22a
    , sShow  = show
    , sSolve = noFail $
          id
    }
