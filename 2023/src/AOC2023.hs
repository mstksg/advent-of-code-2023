{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -Wno-dodgy-exports  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : AOC.Challenge
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.
--

module AOC2023 (
    module AOC
  -- , ChallengeMap
  -- , ChallengeSpec(..), Part(..)
  -- , challengeMap
  -- , lookupSolution
  -- , Day(..), dayInt, mkDay, mkDay_
  -- , solSpec
  -- , charPart
  ) where

import           AOC2023.Day01 as AOC
import           AOC2023.Day02 as AOC
import           AOC2023.Day03 as AOC
import           AOC2023.Day04 as AOC
import           AOC2023.Day05 as AOC
import           AOC2023.Day06 as AOC
import           AOC2023.Day07 as AOC
import           AOC2023.Day08 as AOC
import           AOC2023.Day09 as AOC
import           AOC2023.Day10 as AOC
import           AOC2023.Day11 as AOC
import           AOC2023.Day12 as AOC
import           AOC2023.Day13 as AOC
import           AOC2023.Day14 as AOC
import           AOC2023.Day15 as AOC
import           AOC2023.Day16 as AOC
import           AOC2023.Day17 as AOC
import           AOC2023.Day18 as AOC
import           AOC2023.Day19 as AOC
import           AOC2023.Day20 as AOC
import           AOC2023.Day21 as AOC
import           AOC2023.Day22 as AOC
import           AOC2023.Day23 as AOC
import           AOC2023.Day24 as AOC
import           AOC2023.Day25 as AOC

-- import           AOC.Discover
-- import           AOC.Solver
-- import           Advent
-- import           Control.Monad
-- import           Data.Finite
-- import           Data.Map         (Map)
-- import qualified Data.Map         as M

-- -- | A map of all challenges.
-- challengeMap :: ChallengeMap
-- challengeMap = mkChallengeMap $$(solutionList)

-- -- | Lookup up a solution from a 'ChallengeMap'
-- lookupSolution :: ChallengeSpec -> Map Day (Map Part a) -> Maybe a
-- lookupSolution CS{..} = M.lookup _csPart <=< M.lookup _csDay
