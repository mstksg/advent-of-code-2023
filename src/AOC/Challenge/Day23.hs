{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day23
  ( day23a,
    day23b,
  )
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map as M
import qualified Data.Map.NonEmpty as NEM
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

tile :: Char -> Maybe (Maybe Dir)
tile = \case
  '.' -> Just $ Nothing
  '^' -> Just $ Just North
  'v' -> Just $ Just South
  '>' -> Just $ Just East
  '<' -> Just $ Just West
  _ -> Nothing

data HikeState = HS {hsSeen :: Set Point, hsCurr :: Point}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

expandState :: Map Point (Maybe Dir) -> HikeState -> [HikeState]
expandState mp HS {..} = do
  t <- maybeToList $ M.lookup hsCurr mp
  p <- case t of
    Just d -> pure $ hsCurr + dirPoint' d
    Nothing -> do
      p <- cardinalNeighbs hsCurr
      guard $ p `M.member` mp
      pure p
  guard $ p `S.notMember` hsSeen
  pure $ HS {hsSeen = S.insert p hsSeen, hsCurr = p}

paths :: Map Point (Maybe Dir) -> [HikeState]
paths mp = go (Seq.singleton (HS (S.singleton (V2 1 0)) (V2 1 0)))
  where
    goal = maximumBy (comparing $ view _y) (M.keys mp)
    go = \case
      Seq.Empty -> []
      x Seq.:<| xs
        | hsCurr x == goal -> x : go xs
        | otherwise -> go (xs <> Seq.fromList (expandState mp x))

paths' :: Map Point (Maybe Dir) -> [HikeState]
paths' mp = go S.empty s0
  where
    s0 = Seq.singleton (HS (S.singleton start) start)
    start = V2 1 0
    goal = maximumBy (comparing $ view _y) (M.keys mp)
    go seen = \case
      Seq.Empty -> []
      x Seq.:<| xs
        | hsCurr x == goal -> x : go (S.insert x seen) xs
        | otherwise ->
            go
              (S.insert x seen)
              (xs <> Seq.fromList (filter (`S.notMember` seen) $ expandState mp x))

pathGraph :: Set Point -> Map Point (NEMap Point Int)
pathGraph ps =
  M.mapMaybe NEM.nonEmptyMap
    . M.mapWithKey (\p -> M.fromList . mapMaybe (follow 1 p) . S.toList)
    $ saturated
  where
    follow !n a b
      | b `M.member` saturated = Just (b, n)
      | otherwise =
          M.lookup b skinny >>= \(p, q) ->
            if a == p
              then follow (n+1) b q
              else follow (n+1) b p
    neighbGraph = flip M.fromSet ps $ \p ->
      cardinalNeighbsSet p `S.intersection` ps
    (skinny, saturated) = flip M.mapEither neighbGraph $ \qs ->
      case toList qs of
        [a, b] -> Left (a, b)
        _ -> Right qs

data HikeState2 = HS2 {hs2Seen :: Set Point, hs2Curr :: Point, hs2Length :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- paths2 :: Map Point (NEMap Point Int) -> Int
-- paths2 gr = go 0 (Seq.singleton st0)
--   where
--     st0 = HS2 S.empty (V2 1 0) 0
--     goal = maximumBy (comparing $ view _y) (M.keys gr)
--     go !best = \case
--       Seq.Empty -> best
--       HS2{..} Seq.:<| xs
--         | hs2Curr == goal -> go (max best hs2Length) xs
--         | otherwise ->
--             let nexts = flip mapMaybe (toList (NEM.toList (gr M.! hs2Curr))) \(p, d) -> do
--                   guard $ p `S.notMember` hs2Seen
--                   pure $ HS2 (S.insert p hs2Seen) p (hs2Length + d)
--             in  go best (xs <> Seq.fromList nexts)

-- data BFSState n = BS { _bsClosed  :: !(Map n (Maybe n))  -- ^ map of item to "parent"
--                      , _bsOpen    :: !(Seq n) -- ^ queue
--                      }

-- | map state with the longest path to get to that state. but actually i
-- wonder if transpositions/re-orderings are unique.
paths2 :: Map Point (NEMap Point Int) -> Set HikeState2
paths2 gr = go S.empty S.empty (Seq.singleton st0)
  where
    st0 = HS2 S.empty (V2 1 0) 0
    goal = maximumBy (comparing $ view _y) (M.keys gr)
    go seen res = \case
      Seq.Empty -> res
      s@HS2 {..} Seq.:<| xs
        | hs2Curr == goal -> go (S.insert s seen) (S.insert s res) xs
        | otherwise ->
            let nexts = flip mapMaybe (toList (NEM.toList (gr M.! hs2Curr))) \(p, d) -> do
                  guard $ p `S.notMember` hs2Seen
                  let s' = HS2 (S.insert p hs2Seen) p (hs2Length + d)
                  guard $ s' `S.notMember` seen
                  pure s'
                seen' = seen <> S.fromList nexts
             in go seen' res (xs <> Seq.fromList nexts)

-- case M.minViewWithKey queue of
-- Nothing -> res
-- Just ((x@HS{..},hist), xs)
--   | hsCurr == goal -> go (M.insertWith biggerSize x hist res) queue
--   | otherwise -> _
-- biggerSize x y = x

-- \case

-- pathGraph :: Set Point -> Map Point (NEMap Point Int)

-- pathGraph :: Set Point -> Map Point (NESet Point)
-- pathGraph ps = M.mapMaybe NES.nonEmptySet . flip M.fromSet ps $ \p ->
--   cardinalNeighbsSet p `S.intersection` ps

-- -- | final one should only one, three, or four items
-- reduceGraph :: Map Point (NESet Point) -> Map Point (NESet Point)
-- reduceGraph mp0 = M.mapMaybe unSkinny $ M.unionWith (<>) rest replacements
--   where
--     (skinny, rest) = flip M.mapEither mp0 $ \ps ->
--       case toList ps of
--         [a,b] -> Left (a,b)
--         _ -> Right ps
--     replacements =
--       M.fromList
--         [ newEdge
--           | (p, (q,r)) <- M.toList skinny,
--           let q' = followOut p q
--               r' = followOut p r,
--           newEdge <- [(q', NES.singleton r'), (r', NES.singleton q')]
--         ]
--     followOut :: Point -> Point -> Point
--     followOut a b = case M.lookup b skinny of
--       Just (p,q)
--         | p == a -> followOut b q
--         | otherwise -> followOut b p
--       Nothing -> b
--     unSkinny = NES.nonEmptySet . (`S.difference` M.keysSet skinny) . NES.toSet

-- assembleGraph :: Set Point -> G.Gr Point Int
-- assembleGraph ps = _

day23a :: _ :~> _
day23a =
  MkSol
    { sParse =
        noFail $
          parseAsciiMap tile,
      sShow = show,
      sSolve =
        noFail $
          maximum . map (subtract 1 . S.size . hsSeen) . paths
    }

day23b :: _ :~> _
day23b =
  MkSol
    { sParse = sParse day23a,
      -- sShow = ('\n' :) . unlines . map show . M.toList,
      sShow = show,
      sSolve =
        noFail $
          maximum . map hs2Length . S.toList . paths2 . pathGraph . M.keysSet
          -- reduceGraph . pathGraph . M.keysSet
          -- maximum . map (subtract 1 . S.size . hsSeen) . paths' . fmap (const Nothing)
    }
