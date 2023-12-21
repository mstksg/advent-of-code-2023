{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day18
  ( day18a,
    day18b,
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
import Numeric.Lens (hex)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

shoelace :: NonEmpty Point -> Int
shoelace (z :| zs) = (`div` 2) . abs . sum $ zipWith go (z : zs) (zs ++ [z])
  where
    go (V2 x y) (V2 x' y') = x * y' - y * x'

-- | Maps to possible startpoints and possible endpoints
mkSegments :: Bool -> [(Dir, Int)] -> [V2 (Set Point)]
mkSegments clockwise = snd . mapAccumL go 0
  where
    go p (d, i) = (p', S.fromList . map (buffer d) . consecs <$> V2 p p')
      where
        p' = p + i *^ dirPoint' d
        consecs q = map (\j -> q + j *^ dirPoint' d) [-1,0,1]
    buffer
      | clockwise = \case
          North -> id
          East -> id
          South -> (+ V2 1 0)
          West -> (+ V2 0 1)
      | otherwise = \case
          North -> (+ V2 1 0)
          East -> (+ V2 1 0)
          South -> id
          West -> id

-- | Pick the matching points consecutively
joinSegments :: [V2 (Set Point)] -> Maybe [Point]
joinSegments xs = zipWithM go xs (drop 1 xs)
  where
    go :: V2 (Set Point) -> V2 (Set Point) -> Maybe Point
    go (V2 _ a) (V2 b _) = S.lookupMin (a `S.intersection` b)

day18 :: (String -> Maybe (Dir, Int)) -> [(Dir, Int)] :~> Int
day18 p =
  MkSol
    { sParse = traverse p . lines,
      sShow = show,
      sSolve = \xs -> maximumMay
        [ shoelace (0 :| border)
          | Just border <- joinSegments . (`mkSegments` xs) <$> [False, True]
        ]
    }

parseLineA :: String -> Maybe (Dir, Int)
parseLineA =
  bitraverse (parseDir . head) readMaybe
    <=< listTup
    . take 2
    . words

day18a :: [(Dir, Int)] :~> Int
day18a = day18 parseLineA

parseLineB :: String -> Maybe (Dir, Int)
parseLineB = splitUp . filter isHexDigit <=< (!? 2) . words
  where
    splitUp xs = do
      y : ys <- pure $ reverse xs
      d <- case y of
        '0' -> pure East
        '1' -> pure South
        '2' -> pure West
        '3' -> pure North
        _ -> empty
      i <- preview (reversed . hex) ys
      pure (d, i)

day18b :: [(Dir, Int)] :~> Int
day18b = day18 parseLineB
