{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day25
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 25.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day25 (
    day25a, drop3
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.Containers.ListUtils
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

-- fqt: bzs
-- pbl: jkk zmp

-- floodFill
--     :: Ord a
--     => (a -> Set a)     -- ^ Expansion (be sure to limit allowed points)
--     -> Set a            -- ^ Start points
--     -> Set a            -- ^ Flood filled

day25a :: _ :~> _
day25a = MkSol
    { sParse = 
            fmap (concatMap (\(x,xs) -> (x,) <$> xs)) . traverse (fmap (second words) . listTup . splitOn ": ") . lines
    , sShow  = show
    , sSolve = \xs -> (\(x,y) -> S.size x * S.size y) <$> firstJust isTwoSeparate (drop3 xs)
        -- let forwards = xs
        --     backwards = M.fromListWith (<>)
        --       [ (y,S.singleton b)
        --         | (b,ys) <- M.toList xs
        --       , y <- toList ys
        --         ]
        --     set1 = floodFill (\x -> M.findWithDefault mempty x forwards <> M.findWithDefault mempty x backwards)
        --             (S.singleton "fqt")
        -- in  S.size set1 * S.size (M.keysSet forwards `S.difference` set1)
    }

drop3 :: Ord a => [a] -> [[a]]
drop3 ps = Data.Containers.ListUtils.nubOrd do
  (_,xs) <- select ps
  (_,ys) <- select xs
  (_,zs) <- select ys
  pure zs

isTwoSeparate :: [(String, String)] -> Maybe (Set String, Set String)
isTwoSeparate cs = do
    p0 <- S.lookupMin allItems
    let set1 = floodFill (\x -> M.findWithDefault mempty x fd <> M.findWithDefault mempty x bd)
          (S.singleton p0)
        leftover = allItems `S.difference` set1
    p1 <- S.lookupMin leftover
    let set2 = floodFill (\x -> M.findWithDefault mempty x fd <> M.findWithDefault mempty x bd)
          (S.singleton p1)
    guard $ set1 `S.disjoint` set2
    guard $ (set1 <> set2) == allItems
    pure (set1, set2)
  where
    fd = M.fromListWith (<>) . map (second S.singleton) $ cs
    bd = M.fromListWith (<>) . map (second S.singleton . swap) $ cs
    allItems = M.keysSet fd <> M.keysSet bd
