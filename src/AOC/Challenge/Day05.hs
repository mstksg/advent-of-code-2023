{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day05 (
    day05a
  , day05b
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
import qualified Data.IntervalMap.Strict as IVM
import qualified Data.IntervalSet as IVS
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Data.IntervalMap.Strict (IntervalMap)
import Data.IntervalSet (IntervalSet)
import Data.Interval (Interval)
import qualified Data.Interval as IV

convertSingle :: IntervalMap Int Int -> Int -> Int
convertSingle mp x = maybe x (x+) $ IVM.lookup x mp

convertMany :: IntervalMap Int Int -> IntervalSet Int -> IntervalSet Int
convertMany mp xs = misses <> hits
  where
    tempSet :: IntervalMap Int ()
    tempSet = IVM.fromList . map (,()) . IVS.toList $ xs
    misses = IVM.keysSet $ tempSet IVM.\\ mp
    hits = IVS.fromList
         . map (\(iv, delta) -> IV.mapMonotonic (+ delta) iv)
         . IVM.toList
         $ IVM.intersectionWith const mp tempSet

  --   tempSet :: IntervalMap Int (() -> (Int, Int) -> ())
  --   tempSet = IVM.fromList . map _ . IVS.toList $ xs
  -- case IVM.lookup x mp of
  --   Nothing -> x
  --   Just (dest, src) -> dest + (x - src)

-- convert :: V3 Int -> Int -> Maybe Int
-- convert (V3 dest src len) x
--   | x >= src && x < (src + len) = Just $ dest + (x - src)
--   | otherwise = Nothing

-- convertMany :: [V3 Int] -> Int -> Int
-- convertMany xs i = fromMaybe i . asum $ map (\v -> convert v i) (toList xs)

parseMe :: [String] -> ([Int], [IntervalMap Int Int])
parseMe (x:xs) = (map read (tail (words x)),
                  map (IVM.fromList . map (mkIval . fmap read . fromJust . listV3 . words) . tail) $
                    splitOn [""] (tail xs)
                 )
  where
    mkIval (V3 dest src len) = (IV.Finite src IV.<=..< IV.Finite (src + len), dest - src)

-- seeds: 79 14 55 13

-- seed-to-soil map:
-- 2023441036 2044296880 396074363
-- 2419515399 3839972576 454994720
-- 274688417 699823315 258919718
-- 533608135 0 431744151
-- 965352286 431744151 161125324
-- 3391658630 2936663910 903308666
-- 200749950 1177785526 73938467
-- 2874510119 1440389999 315892137
-- 1916089471 2440371243 20593195
-- 0 977035576 200749950
-- 1936682666 1957538510 86758370
-- 1440389999 2902130623 34533287
-- 1126477610 592869475 106953840
-- 3190402256 1756282136 201256374
-- 1474923286 2460964438 441166185
-- 1233431450 958743033 18292543


-- seeds: 104847962 3583832 1212568077 114894281 3890048781 333451605 1520059863 217361990 310308287 12785610 3492562455 292968049 1901414562 516150861 2474299950 152867148 3394639029 59690410 862612782 176128197

day05a :: _ :~> _
day05a = MkSol
    { sParse = noFail $
                parseMe . lines
    , sShow  = show
    , sSolve = noFail $ \(s0, process) ->
                minimum $ M.fromSet (\i -> foldl' (flip convertSingle) i process) (S.fromList s0)
    }
  -- where
  --   go i xs = fromMaybe i . asum $ map (\v -> convert v i) (toList xs)

day05b :: _ :~> _
day05b = MkSol
    { sParse = sParse day05a
    , sShow  = show
    -- , sSolve = noFail id
    -- , sSolve = noFail $
    --             id
    , sSolve = \(s0, process) ->
                fromFinite . IV.lowerBound . IVS.span $
                  foldl' (flip convertMany)
                  -- M.fromSet (\i -> foldl' go i process) $
                    (IVS.fromList $
                      map ((\(x,y) -> IV.Finite x IV.<=..< IV.Finite (x + y)) . fromJust . listTup)
                        $ chunksOf 2 s0
    -- mkIval (V3 dest src len) = (IV.Finite src IV.<=..< IV.Finite (src + len), dest - src)
                    )
                    process
    }
  where
    fromFinite = \case
      IV.Finite x -> Just x
      _ -> Nothing

    -- go i xs = fromMaybe i . asum $ map (\v -> convert v i) (toList xs)
    -- go2 curr xs = S.map (convertMany xs) curr
