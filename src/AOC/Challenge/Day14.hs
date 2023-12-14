{-# OPTIONS_GHC -Wno-unused-imports   #-}
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

module AOC.Challenge.Day14 (
    day14a
  , day14b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import           Data.Group
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

shiftNorth :: Map Point Bool -> Map Point Bool
shiftNorth mp = M.fromList
    [ (V2 x y, p) 
      | (x, ys) <- IM.toList fallens
      , (y, p) <- IM.toList ys
      ]
  where
    minRow = minimum (view _y <$> M.keys mp)
    cols = IM.fromListWith (<>)
                [ (x, IM.singleton y k)
                  | (V2 x y, k) <- M.toList mp
                ]
    fallens :: IntMap (IntMap Bool)
    fallens = cols <&> \col ->
      let fallen :: IntMap Int
          fallen = foldl' (\curr (i, item) ->
            case item of 
              False -> let (j, count) = IM.findMax curr
                       in IM.insert j (count + 1) curr
              True  -> IM.insert (i + 1) 0 curr
                )
                (IM.singleton minRow 0) (IM.toList col)
          fallenPos = IS.fromList 
            [ j
              | (i, n) <- IM.toList fallen
              , j <- [i..i+n-1]
            ]
       in IM.filter id col <> IM.fromSet (const False) fallenPos

score :: Map Point Bool -> Int
score mp = sum $ M.keys (M.filter not mp) <&> \(V2 _ y) -> maxRow - y
  where 
    maxRow = maximum (view _y <$> M.keys mp)+1

shiftDir :: Dir -> Map Point Bool -> Map Point Bool
shiftDir d = M.mapKeys (rotPoint (invert d))
           . shiftNorth
           . M.mapKeys (rotPoint d)

shiftCycle :: Map Point Bool -> Map Point Bool
shiftCycle = M.mapKeys (rotPoint West)
           . shiftNorth
           . M.mapKeys (rotPoint West)
           . shiftNorth
           . M.mapKeys (rotPoint West)
           . shiftNorth
           . M.mapKeys (rotPoint West)
           . shiftNorth

-- rotSet :: Map Point Bool -> Map Point Bool
-- rotSet = reshift . M.mapKeys (rotate West)
--   where
--     reshit mp = M.mapKeysMonotonic (subtract mn) mp
--       where
--         Just mn = minCorner' (M.keys mp)
    -- Just mn -> S.mapMonotonic (liftA2 subtract mn) ps

-- -- | Rotate a point by a direction
-- rotPoint :: Num a => Dir -> V2 a -> V2 a

day14a :: _ :~> _
day14a = MkSol
    { sParse = noFail $
                parseAsciiMap $ \case
                  'O' -> Just False
                  '#' -> Just True
                  _ -> Nothing
    -- , sShow  = ('\n':) . displayAsciiMap '.' . fmap (\case False -> 'O'; True -> '#')
    , sShow  = show
    , sSolve = noFail $ score . shiftNorth
    -- , sSolve = noFail $ \xs ->
    --       let cols = IM.fromListWith (<>)
    --             [ (x, IM.singleton y k)
    --               | (V2 x y, k) <- M.toList xs
    --             ]
    --           maxRow = maximum (view _y <$> M.keys xs)+1
    --        in  sum . flip map (IM.elems cols) $ \col ->
    --             let fallen :: IntMap Int
    --                 fallen = foldl' (\curr (i, item) ->
    --                   case item of 
    --                     False -> let (j, count) = IM.findMax curr
    --                              in IM.insert j (count + 1) curr
    --                     True  -> IM.insert (i + 1) 0 curr
    --                       )
    --                       (IM.singleton 0 0) (IM.toList col)
    --              in sum
    --                   [ maxRow - j
    --                     | (i, n) <- IM.toList fallen
    --                    , j <- [i..i+n-1]
    --                   ]
          -- firstRepeated $ iterate step xs
-- firstRepeated :: Ord a => [a] -> Maybe a
-- firstRepeated = firstRepeatedBy id
    }
  -- where
    -- step mp = 

day14b :: _ :~> _
day14b = MkSol
    { sParse = sParse day14a
    , sShow  = show
    -- , sShow  = ('\n':) . displayAsciiMap '.' . fmap (\case False -> 'O'; True -> '#')
    -- , sSolve = Just . (!! 1) . iterate shiftCycle
    -- , sSolve = Just . map score . take 50 . iterate shiftCycle
    , sSolve = fmap loopMath . findLoopBy id . iterate shiftCycle
-- findLoopBy :: Ord a => (b -> a) -> [b] -> Maybe (V2 (Int, b))
-- firstRepeated :: Ord a => [a] -> Maybe a
      -- strictIterate shiftCycle mp !! 10000
            -- rotSet . rotSet . rotSet . shiftNorth . rotSet . shiftNorth $ mp
    }
  where
    loopMath (V2 (i, x) (j, _)) = score $ iterate shiftCycle x !!! leftover
      where
        leftover = (1000000000 - i) `mod` (j - i)
-- findLoopBy :: Ord a => (b -> a) -> [b] -> Maybe (V2 (Int, b))
