{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day16
  ( day16a,
    day16b,
  )
where

import AOC.Prelude
import Data.Functor.Foldable hiding (fold)
import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
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
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

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

unProcess :: Tile -> Dir -> Set Dir
unProcess = \case
  SplitNS ->
    S.fromList . \case
      North -> [North, East, West]
      South -> [South, East, West]
      East -> []
      West -> []
  SplitEW ->
    S.fromList . \case
      North -> []
      South -> []
      East -> [East, North, South]
      West -> [West, North, South]
  MirrorNE ->
    S.singleton . \case
      North -> East
      South -> West
      East -> North
      West -> South
  MirrorNW ->
    S.singleton . \case
      North -> West
      South -> East
      East -> South
      West -> North

data PathTrie k v = PTNode !v (Map k (PathTrie k v))
  deriving stock (Show, Generic, Eq, Ord)

instance (NFData v, NFData k) => NFData (PathTrie k v)

makeBaseFunctor ''PathTrie

stepTrie :: Map Point Tile -> (Set (Point, Dir), (Point, Dir)) -> PathTrieF Dir Point (Set (Point, Dir), (Point, Dir))
stepTrie mp = go
  where
    Just bb = boundingBox' (M.keys mp)
    go (seen, (p, d))
      | (p, d) `S.member` seen = PTNodeF p M.empty
      | otherwise =
          PTNodeF p $
            M.fromList
              [ (d', (S.insert (p, d) seen, (p', d')))
                | (p', d') <- pd',
                  inBoundingBox bb p'
              ]
      where
        pd' = case M.lookup p mp of
          Just t -> process t d <&> \newDir -> (p + dirPoint' newDir, newDir)
          Nothing -> pure (p + dirPoint' d, d)

triePoints :: PathTrieF Dir Point (Set Point) -> Set Point
triePoints (PTNodeF p xs) = S.insert p $ fold xs

runMap :: Map Point Tile -> [[Point]]
runMap mp = go 0 East
  where
    Just bb = boundingBox' (M.keys mp)
    go !p !d
      | inBoundingBox bb p = map (p :) case M.lookup p mp of
          Nothing -> go (p + dirPoint' d) d
          Just t -> do
            d' <- process t d
            go (p + dirPoint' d') d'
      | otherwise = pure []

-- | this would have been too good to be true lol
fillMap :: Map Point Tile -> Map Point (Map Dir Bool)
fillMap mp = res
  where
    res = M.fromSet go (fillBoundingBox' (M.keys mp))
    go p = flip M.fromSet (NES.toSet allDirSet) \d ->
      let q = p + dirPoint' (d <> South)
          d' = case M.lookup q mp of
            Nothing -> S.singleton d
            Just t -> unProcess t d
       in case M.lookup q res of
            Nothing -> False
            Just ds -> or $ ds `M.restrictKeys` d'

searchPoints :: Map Point Tile -> Point -> Dir -> Set Point
searchPoints mp p0 d0 = go S.empty (S.singleton (p0, d0))
  where
    Just bb = boundingBox' (M.keys mp)
    go seen queue = case S.minView queue of
      Nothing -> S.map fst seen
      Just ((p, d), queue') ->
        let stepped = filter (inBoundingBox bb . fst) case M.lookup p mp of
              Nothing -> [(p + dirPoint' d, d)]
              Just t -> process t d <&> \d' -> (p + dirPoint' d', d')
            newSteps = S.fromList stepped `S.difference` seen
         in go (S.insert (p, d) seen) (newSteps <> queue')

day16a :: _ :~> _
day16a =
  MkSol
    { sParse = Just . parseAsciiMap classify,
       sShow  = show,
      -- sShow = ('\n' :) . displayAsciiSet '.' '#',
      -- , sSolve = noFail $ fillMap
      -- , sSolve = noFail $ \mp -> ana (stepTrie mp) (0, East) :: PathTrie Dir Point
      -- , sSolve = noFail $ \mp -> S.size $ hylo triePoints (stepTrie mp) (S.empty, (0, East))
      sSolve = noFail $ \mp -> S.size $ searchPoints mp 0 East
      -- , sSolve = Just . S.size . foldMap S.fromList . runMap
    }

day16b :: _ :~> _
day16b =
  MkSol
    { sParse = sParse day16a,
      sShow = show,
      sSolve = \mp -> do
        V2 (V2 xMin yMin) (V2 xMax yMax) <- boundingBox' (M.keys mp)
        let allStarts = concat
              [ (,South) . (`V2` yMin) <$> [xMin .. xMax]
              , (,North) . (`V2` yMax) <$> [xMin .. xMax]
              , (,East) . V2 xMin <$> [yMin .. yMax]
              , (,West) . V2 xMax <$> [yMin .. yMax]
              ]
        maximumMay $ S.size . uncurry (searchPoints mp) <$> allStarts


    }
