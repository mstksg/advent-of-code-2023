{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day12
  ( day12a,
    day12b,
    combosOf,
  )
where

import AOC.Prelude
import Data.Bitraversable
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

-- #.#.### 1,1,3

parseChar = \case
  '#' -> Just $ Just True
  '.' -> Just $ Just False
  '?' -> Just $ Nothing
  _ -> Nothing

-- ???.### 1,1,3

classify :: [Bool] -> [Int]
classify = filter (> 0) . map length . splitOn [False]

combosOf :: Int -> [[(Int, Bool)]]
combosOf x = cacheMap IM.! x
  where
    cacheMap =
      IM.fromList
        [ ( i,
            (map (\qs -> (length qs, head qs)) . group)
              <$> replicateM i [False, True]
          )
          | i <- [0 .. 100]
        ]

expandOut :: [(Int, Maybe Bool)] -> [[(Int, Bool)]]
expandOut = fmap (map rematch . fmap concat) . traverse $ \(i, b) -> case b of
  Nothing -> combosOf i
  Just x -> [[(i, x)]]
  where
    -- rematch [] = []
    rematch ((i,x0):xs) = go i x0 xs
      where
        go !n x = \case
          [] -> [(n,x)]
          (j,y):ys
            | x == y -> go (n + j) x ys
            | otherwise -> (n,x) : go j y ys
    -- rematch = map (\qs -> (length qs, head qs)) . group . concatMap (\(i, x) -> replicate i x)

-- expandOutMatches :: [Int] -> [(Int, Maybe Bool)] -> Int
--   -- [[(Int, Bool)]]
-- expandOutMatches = \case
--     [] -> \case
--       [] -> 1
--       (_, Just False):xs -> expandOutMatches [] xs
--       (_, Just True):xs -> 0
--     p:ps -> \case
--       [] -> 0
--       (n, Just False):xs -> _
--       (n, Just True):xs -> _
--     -- p:ps -> \case
--     --   [] ->
--   -- fmap concat . traverse $ \(i, b) -> case b of
--   -- Nothing -> combosOf i
--   -- Just x  -> [[(i, x)]]

day12a :: _ :~> _
day12a =
  MkSol
    { sParse =
        traverse
          ( bitraverse (traverse parseChar) (traverse (readMaybe @Int) . splitOn ",")
              <=< listTup
              . words
          )
          . lines,
      sShow = show,
      sSolve = noFail $
        fmap sum . map $ \(xs :: [Maybe Bool], pat :: [Int]) ->
          countTrue ((== pat) . classify) $
            traverse (\case Nothing -> [False, True]; Just x -> [x]) xs
    }

-- .??..??...?##..??..??...?##..??..??...?##..??..??...?##..??..??...?##.
-- 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3

-- | New case: true = #, false = ?
chunkUp :: [Maybe Bool] -> [Seq Bool]
chunkUp = go
  where
    go = \case
      [] -> []
      Just False:xs -> chunkUp xs
      Just True:xs -> eatUp (Seq.singleton True) xs
      Nothing:xs -> eatUp (Seq.singleton False) xs
    eatUp qs = \case
      [] -> [qs]
      Just False:xs -> qs : go xs
      Just True:xs -> eatUp (qs Seq.:|> True) xs
      Nothing:xs -> eatUp (qs Seq.:|> False) xs

chunkPatterns :: Seq Bool -> Set [Int]
chunkPatterns = S.fromList . map reChunk . traverse (\case True -> [True]; False -> [False,True])
  where
    reChunk :: Seq Bool -> [Int]
    reChunk = traceShowId . map length . filter (not . null) . splitOn [False] . toList

-- consumeChunkPatterns :: [Int] -> [Set (NonEmpty Int)] -> Int
-- consumeChunkPatterns = \case
--     [] -> \case
--       [] -> 1
--       -- at least one more to consume, so must fail
--       _:_ -> 0
--     n:ns -> \case
--       [] -> 0
--       x:xs -> _
--         -- product
--         -- [ _
--         -- ]


day12b :: _ :~> _
day12b =
  MkSol
    { sParse = sParse day12a,
      sShow = show,
      sSolve = noFail $
        map $ \(xs :: [Maybe Bool], pat :: [Int]) ->
          -- fmap sum . map $ \(xs :: [Maybe Bool], pat :: [Int]) ->
          let pat' :: [Int]
              pat' = concat $ replicate 5 pat
              xs' :: [Maybe Bool]
              xs' = intercalate [Nothing] $ replicate 5 xs
          in  head $ chunkPatterns <$> chunkUp xs'
           -- in countTrue (matchesPat pat') $
           --      expandOut . map (\qs -> (length qs, head qs)) $
           --        group xs'
                  -- countTrue ((== pat') . classify) $
                  --        traverse (\case Nothing -> [False,True]; Just x -> [x]) xs'
    }
  where
    matchesPat pat = (== pat) . map fst . filter snd
