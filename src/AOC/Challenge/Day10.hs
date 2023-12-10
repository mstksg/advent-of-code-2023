{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day10
  ( day10a,
    day10b,
  )
where

import AOC.Prelude
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.Graph.Inductive as G
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

-- data Path k v = Node v (NEMap k (Path k v)) | Leaf
--   deriving stock (Functor, Show, Generic)

-- instance (NFData k, NFData v) => NFData (Path k v)

-- makeBaseFunctor ''Path

-- unfoldLoop ::
--   Map Point (Set Dir) ->
--   (Set Point, Point) ->
--   PathF Dir Point (Set Point, Point)
-- unfoldLoop mp (seen, x) = case NEM.nonEmptyMap subMap of
--   Nothing -> LeafF
--   Just sm -> NodeF x sm
--   where
--     subMap = case M.lookup x mp of
--       Nothing -> M.empty
--       Just ds ->
--         M.mapMaybe id
--           . M.fromSet
--             ( \d ->
--                 let y = dirPoint' d + x
--                  in guard (y `S.notMember` seen && M.member y mp)
--                       $> (x `S.insert` seen, y)
--             )
--           $ ds

-- maxLoop :: PathF Dir a Int -> Int
-- maxLoop = \case
--   LeafF -> 1
--   NodeF _ xs -> maybe 1 (+ 1) $ maximumMay xs

-- loopPaths :: PathF Dir Point [[(Dir, Point)]] -> [[(Dir, Point)]]
-- loopPaths = \case
--   LeafF -> [[]]
--   NodeF p xs -> concatMap (\(d, ps) -> map ((d, p) :) ps) . toList $ NEM.toList xs

prepareMap :: Map Point (Maybe (Set Dir)) -> Maybe (Point, Map Point (Set Dir))
prepareMap mp = do
    startPos <- lookup Nothing (swap <$> M.toList mp)
    let dirs = flip S.filter (NES.toSet allDirSet) \d ->
          case join $ M.lookup (startPos + dirPoint' d) mp of
            Nothing -> False
            Just ds -> (d <> South) `S.member` ds
    pure (startPos, fromMaybe dirs <$> mp)

-- | the Dir is the outbound dir
followPath :: Map Point (Set Dir) -> Point -> [(Dir, Point)]
followPath mp startPoint = unfoldr go (startPoint, Nothing)
  where
    go (currPos, lastPos)
      | currPos == startPoint && isJust lastPos = Nothing
      | otherwise = listToMaybe do
          d <- foldMap S.toList $ M.lookup currPos mp
          let nextPos = dirPoint' d + currPos
          for_ lastPos \p -> guard (nextPos /= p)
          pure ((d, currPos), (nextPos, Just currPos))
          -- let pd = flip mapMaybe ds \d ->
          --       currPos + dirPoint' d
          -- let ps = map ((+ currPos) . dirPoint') ds

          -- p <- S.lookupMin . S.map ((+ currPos) . dirPoint')
          -- d' <- S.lookupMin . S.delete (d <> South) =<< M.lookup y mp
          -- pure (d', y, e)
      -- where
      --   y = x + dirPoint' d
          -- d' <- S.findMin . S.delete  $ M.lookup x mp
      -- $ listToMaybe do
      -- guard $ x `S.notMember` seen
      -- d <- traceShowId $ foldMap S.toList $ M.lookup x mp
      -- let y = x + dirPoint' d
      -- pure ((d, x), (S.insert x seen, y))

day10a :: _ :~> _
day10a =
  MkSol
    { sParse = noFail $
        parseAsciiMap \case
          '|' -> Just $ Just $ S.fromList [North, South]
          '-' -> Just $ Just $ S.fromList [East, West]
          'L' -> Just $ Just $ S.fromList [North, East]
          'J' -> Just $ Just $ S.fromList [North, West]
          '7' -> Just $ Just $ S.fromList [West, South]
          'F' -> Just $ Just $ S.fromList [East, South]
          'S' -> Just Nothing
          _ -> Nothing,
      sShow = show,
      sSolve = \mp -> do
        (startPos, mp') <- prepareMap mp
        pure $ length (followPath mp' startPos) `div` 2
        -- pure $ length (followPath mp' startPos) `div` 2
    }

bothDirs :: [(Dir, a)] -> [((Dir, Dir), a)]
bothDirs xs = zipWith go xs (drop 1 $ cycle xs)
  where
    go (d', _) (d, p) = ((d, d'), p)

day10b :: _ :~> _
day10b =
  MkSol
    { sParse = sParse day10a,
      sShow = show,
      -- sShow = ('\n':) . displayAsciiMap ' ',
      -- sShow = ('\n':) . displayAsciiSet '.' '#',
      sSolve = \mp -> do
        (startPos, mp') <- prepareMap mp
        let path = followPath mp' startPos 
        -- pure $ length (followPath mp' startPos) `div` 2
        -- startPos <- lookup Nothing (swap <$> M.toList mp)
        -- let mp' = fromMaybe (NES.toSet allDirSet) <$> mp
        -- path : _ <-
        --   pure $
        --     hylo loopPaths (unfoldLoop mp') (S.empty, startPos)
        let loopPoints = S.fromList $ map snd path
        bb <- boundingBox' loopPoints
        -- V2 (V2 xMin yMin) (V2 xMax yMax) <- boundingBox' loopPoints
        let rollUp turner = foldl' go S.empty (bothDirs path)
                                  `S.difference` loopPoints
              where
                go seen ((dIn, dOut), p) = seen
                                        <> S.fromList (lineFrom dIn)
                                        <> S.fromList (lineFrom dOut)
                  where
                    lineFrom :: Dir -> [Point]
                    lineFrom d = 
                        takeWhile (\p' -> inBoundingBox bb p' && p' `S.notMember` loopPoints)
                      . drop 1
                      $ iterate (+ dirPoint' (d <> turner)) p
            -- rolled = minimumBy (comparing S.size) [rollUp East, rollUp West]
        minimumMay . map S.size $ [rollUp East, rollUp West]
        -- pure path
        -- pure $ rollUp East `S.difference` M.keysSet mp'
        -- pure $ S.size <$> [rollUp East, rollUp West]
        -- pure rolled
        -- pure $
        --   M.fromSet (const '▋') rolled
        --   <> fmap unParse mp'
        -- let loopPoints = S.fromList $ map snd path
        -- V2 (V2 xMin yMin) (V2 xMax yMax) <- boundingBox' loopPoints
        -- pure $ flip countTrue (V2 <$> [xMin .. xMax] <*> [yMin .. yMax]) \p@(V2 x y) ->
        --   ((p `S.notMember` loopPoints) &&) $ flip all allDir \d ->
        --     let lineToBorder = S.fromList case d of
        --           North -> (`V2` y) <$> [xMin .. x - 1]
        --           South -> (`V2` y) <$> [x + 1 .. xMax]
        --           East -> V2 x <$> [y + 1 .. yMax]
        --           West -> V2 x <$> [yMin .. y - 1]
        --      in not . S.null $ lineToBorder `S.intersection` loopPoints
            -- North -> S.null $ S.intersection loopPoints

            -- boundingBox' :: (Foldable f, Applicative g, Ord a) => f (g a) -> Maybe (V2 (g a))
            -- boundingBox' = fmap boundingBox . NE.nonEmpty . toList
            -- pure $
            --   -- hylo maxLoop (unfoldLoop mp') (S.empty, startPos) `div` 2
            --   hylo loopPaths (unfoldLoop mp') (S.empty, startPos)
            -- pure path1
            -- pure $
            --   hylo maxLoop (unfoldLoop mp') (S.empty, startPos) `div` 2
    }


_unParse :: Set Dir -> Char
_unParse ds
  | ds == S.fromList [North, South] = '│'
  | ds == S.fromList [East, West] = '─'
  | ds == S.fromList [North, East] = '└'
  | ds == S.fromList [North, West] = '┘'
  | ds == S.fromList [West, South] = '┐'
  | ds == S.fromList [East, South] = '┌'
  | otherwise = '?'
