-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
module AOC.Challenge.Day16
  ( day16a,
    day16b,
  )
where

import AOC.Common.Point (Dir (..), Point, boundingBox, dirPoint', inBoundingBox, parseAsciiMap)
import AOC.Solver (noFail, (:~>) (..))
import Control.DeepSeq (NFData)
import Data.Functor ((<&>))
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEM
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Linear.V2 (V2 (..))
import Safe (maximumMay)

data Tile = SplitNS | SplitEW | MirrorNE | MirrorNW
  deriving stock (Show, Generic, Eq, Ord)

instance NFData Tile

classify :: Char -> Maybe Tile
classify = \case
  '|' -> Just SplitNS
  '-' -> Just SplitEW
  '/' -> Just MirrorNE
  '\\' -> Just MirrorNW
  _ -> Nothing

process :: Tile -> Dir -> [Dir]
process = \case
  SplitNS -> \case
    North -> [North]
    South -> [South]
    East -> [North, South]
    West -> [North, South]
  SplitEW -> \case
    North -> [East, West]
    South -> [East, West]
    East -> [East]
    West -> [West]
  MirrorNE ->
    (: []) . \case
      North -> East
      South -> West
      East -> North
      West -> South
  MirrorNW ->
    (: []) . \case
      North -> West
      South -> East
      East -> South
      West -> North

searchPoints :: NEMap Point Tile -> Point -> Dir -> Set Point
searchPoints mp p0 d0 = go S.empty (S.singleton (p0, d0))
  where
    bb = boundingBox (NEM.keys mp)
    go seen queue = case S.minView queue of
      Nothing -> S.map fst seen
      Just ((p, d), queue') ->
        let stepped = filter (inBoundingBox bb . fst) case NEM.lookup p mp of
              Nothing -> [(p + dirPoint' d, d)]
              Just t -> process t d <&> \d' -> (p + dirPoint' d', d')
            newSteps = S.fromList stepped `S.difference` seen
         in go (S.insert (p, d) seen) (newSteps <> queue')

day16a :: NEMap Point Tile :~> Int
day16a =
  MkSol
    { sParse = NEM.nonEmptyMap . parseAsciiMap classify,
      sShow = show,
      sSolve = noFail $ \mp -> S.size $ searchPoints mp 0 East
    }

day16b :: NEMap Point Tile :~> Int
day16b =
  MkSol
    { sParse = sParse day16a,
      sShow = show,
      sSolve = \mp -> do
        let V2 (V2 xMin yMin) (V2 xMax yMax) = boundingBox (NEM.keys mp)
            allStarts =
              concat
                [ (,South) . (`V2` yMin) <$> [xMin .. xMax],
                  (,North) . (`V2` yMax) <$> [xMin .. xMax],
                  (,East) . V2 xMin <$> [yMin .. yMax],
                  (,West) . V2 xMax <$> [yMin .. yMax]
                ]
        maximumMay $ S.size . uncurry (searchPoints mp) <$> allStarts
    }
