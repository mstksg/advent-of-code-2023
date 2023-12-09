{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day09 (
    day09a
  , day09b
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

day09a :: _ :~> _
day09a = MkSol
    { sParse = traverse (traverse (readMaybe @Int) . words) . lines
    , sShow  = show
    , sSolve = noFail $
            sum . map getNext
    }
  where
    getNext = sum
            . map last
            -- . foldr1 (\diffs xs -> zipWith (+) xs diffs)
            -- . over _head (\(x:xs) -> x:x:xs)
            -- . reverse
            . iterateMaybe (\xs -> let ys = zipWith (-) (drop 1 xs) xs in guard (not $ all (== 0) ys) $> ys)
-- loopMaybe
--     :: (a -> Maybe a)
--     -> a
--     -> a

day09b :: _ :~> _
day09b = MkSol
    { sParse = sParse day09a
    , sShow  = show
    , sSolve = noFail $
            sum . map getPrev
    }
  where
    getPrev = sum
            . map last
            -- . foldr1 (\diffs xs -> zipWith (+) xs diffs)
            -- . over _head (\(x:xs) -> x:x:xs)
            -- . reverse
            . iterateMaybe (\xs -> let ys = zipWith (-) (drop 1 xs) xs in guard (not $ all (== 0) ys) $> ys)
            . reverse
