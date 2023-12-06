{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day06
  ( day06a,
    day06b,
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

-- Time:      7  15   30
-- Distance:  9  40  200

getWays :: Int -> Int -> Int
getWays tm dm =
  length
    [ ()
      | t <- [0 .. tm],
        let vf = t
            restT = tm - vf
            d = restT * vf,
        d > dm
    ]

getWays' :: Double -> Double -> Int
getWays' tm dm = floor $
  0.5 * (tm + sqrt(tm^2 - 4*dm)) - 0.5 * (tm - sqrt(tm^2 - 4*dm))
-- 1/2 (m - sqrt(m^2 - 4 d))<t<1/2 (sqrt(m^2 - 4 d) + m) and d<m^2/4 and m element R
  -- length
  --   [ ()
  --     | t <- [0 .. tm],
  --       let vf = t
  --           restT = tm - vf
  --           d = restT * vf,
  --       d > dm
  --   ]


-- (tm - t) * t = tm * t - t ^2 > dm
--
-- tm * t - t^2 > dm
--

day06a :: _ :~> _
day06a =
  MkSol
    { sParse =
        noFail $
          map (fromJust . listTup) . transpose . map (map (read @Int) . tail . words) . lines,
      sShow = show,
      sSolve =
        noFail $
          product . map (uncurry getWays)
    }

day06b :: _ :~> _
day06b =
  MkSol
    { sParse = noFail $
          fromJust . listTup . map (read @Int . filter isDigit) . lines,
      sShow = show,
      sSolve =
        noFail $
          \(x, y) -> getWays' (fromIntegral x) (fromIntegral y)
    }
