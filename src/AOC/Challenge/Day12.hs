{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day12
  ( day12a,
    day12b,
    combosOf,
    eatState,
    eater,
  )
where

import AOC.Prelude
import Control.Monad.Writer
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
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

-- #.#.### 1,1,3

-- | true = #, false = .
parseChar = \case
  '#' -> Just $ Just True
  '.' -> Just $ Just False
  '?' -> Just $ Nothing
  _ -> Nothing

-- ???.### 1,1,3

classify :: [Bool] -> [Int]
classify = filter (> 0) . map length . splitOn [False]

combosOf :: Int -> [[(Int, Bool)]]
combosOf x = cacheMap IM.! x
  where
    cacheMap =
      IM.fromList
        [ ( i,
            (map (\qs -> (length qs, head qs)) . group)
              <$> replicateM i [False, True]
          )
          | i <- [0 .. 100]
        ]

expandOut :: [(Int, Maybe Bool)] -> [[(Int, Bool)]]
expandOut = fmap (map rematch . fmap concat) . traverse $ \(i, b) -> case b of
  Nothing -> combosOf i
  Just x -> [[(i, x)]]
  where
    -- rematch [] = []
    rematch ((i, x0) : xs) = go i x0 xs
      where
        go !n x = \case
          [] -> [(n, x)]
          (j, y) : ys
            | x == y -> go (n + j) x ys
            | otherwise -> (n, x) : go j y ys

-- rematch = map (\qs -> (length qs, head qs)) . group . concatMap (\(i, x) -> replicate i x)

-- expandOutMatches :: [Int] -> [(Int, Maybe Bool)] -> Int
--   -- [[(Int, Bool)]]
-- expandOutMatches = \case
--     [] -> \case
--       [] -> 1
--       (_, Just False):xs -> expandOutMatches [] xs
--       (_, Just True):xs -> 0
--     p:ps -> \case
--       [] -> 0
--       (n, Just False):xs -> _
--       (n, Just True):xs -> _
--     -- p:ps -> \case
--     --   [] ->
--   -- fmap concat . traverse $ \(i, b) -> case b of
--   -- Nothing -> combosOf i
--   -- Just x  -> [[(i, x)]]

solve1 :: [Maybe Bool] -> [Int] -> Int
solve1 xs pat =
  countTrue ((== pat) . classify) $
    traverse (\case Nothing -> [False, True]; Just x -> [x]) xs

solve2 :: [Maybe Bool] -> [Int] -> Int
solve2 xs pat = length . eatRuns pat $ chunkUp xs

solve3 :: [Maybe Bool] -> [Int] -> Int
solve3 xs pat = eatRuns2 pat $ chunkUp xs

solve4 :: [Maybe Bool] -> [Int] -> Int
solve4 xs pat = eatRuns3 pat $ chunkUp xs

-- countTrue ((== pat) . classify) $
--           traverse (\case Nothing -> [False, True]; Just x -> [x]) xs

day12a :: _ :~> _
day12a =
  MkSol
    { sParse =
        traverse
          ( bitraverse (traverse parseChar) (traverse (readMaybe @Int) . splitOn ",")
              <=< listTup
              . words
          )
          . lines,
      sShow = show,
      sSolve =
        noFail $
          -- map (\(a,b) -> (solve1 a b, solve2 a b))
          sum . map (uncurry solve4)
          -- sSolve = noFail $
          -- fmap sum . map $ \(xs :: [Maybe Bool], pat :: [Int]) ->
          --   countTrue ((== pat) . classify) $
          --     traverse (\case Nothing -> [False, True]; Just x -> [x]) xs
          -- sSolve =
          --   fmap (fmap sum) . traverse $ \(xs :: [Maybe Bool], pat :: [Int]) ->
          --     -- fmap sum . map $ \(xs :: [Maybe Bool], pat :: [Int]) ->
          --     let pat' :: [Int]
          --         pat' = concat $ replicate 5 pat
          --         xs' :: [Maybe Bool]
          --         xs' = intercalate [Nothing] $ replicate 5 xs
          --      -- in traverse NESeq.nonEmptySeq (chunkUp xs)
          --      in length . eatRuns pat <$> traverse NESeq.nonEmptySeq (chunkUp xs)
    }

-- .??..??...?##..??..??...?##..??..??...?##..??..??...?##..??..??...?##.
-- 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3

-- | New case: true = #, false = ?
chunkUp :: [Maybe Bool] -> [NESeq Bool]
chunkUp = go
  where
    go = \case
      [] -> []
      Just False : xs -> go xs
      Just True : xs -> eatUp (NESeq.singleton True) xs
      Nothing : xs -> eatUp (NESeq.singleton False) xs
    eatUp qs = \case
      [] -> [qs]
      Just False : xs -> qs : go xs
      Just True : xs -> eatUp (qs NESeq.|> True) xs
      Nothing : xs -> eatUp (qs NESeq.|> False) xs

chunkPatterns :: Seq Bool -> Set [Int]
chunkPatterns = S.fromList . map reChunk . traverse (\case True -> [True]; False -> [False, True])
  where
    reChunk :: Seq Bool -> [Int]
    reChunk = traceShowId . map length . filter (not . null) . splitOn [False] . toList

-- consumeChunkPatterns :: [Int] -> [Set (NonEmpty Int)] -> Int
-- consumeChunkPatterns = \case
--     [] -> \case
--       [] -> 1
--       -- at least one more to consume, so must fail
--       _:_ -> 0
--     n:ns -> \case
--       [] -> 0
--       x:xs -> _
--         -- product
--         -- [ _
--         -- ]

-- | Given a run and a seq, how many was could the Seq be eaten by the run.
-- Note that we go this way and not the other way around because runs can
-- leave incomplete sequences but not vice versa, since runs cannot go past
-- sequence boundaries.  however there is the case we have to worry about
-- where the run does not consume any of the sequence at all.
--
-- Just = run was used up, Nothing = seq consumed but run was skipped (ie, all False)

-- ????????#??? 2,3

-- should be simpelr: just check the index of the first True and see if the
-- run fits within the rest of the chunk.
eater ::
  Int ->
  Seq Bool ->
  -- | the max point where you can eat up to
  [Int]
eater n xs
  | Seq.length xs < n = []
  | otherwise =
      mapMaybe (uncurry go) $
        zip
          [0 .. Seq.length (Seq.takeWhileL not xs)]
          (toList $ Seq.tails (Seq.drop n xs))
  where
    go i = \case
      Seq.Empty -> Just i
      False :<| _ -> Just i
      True :<| _ -> Nothing

-- _ <$> Seq.tails (drop n xs)

-- case Seq.elemIndexL True xs of
-- -- only in this case can we go up to the very end
-- Nothing -> IS.fromList [0 .. Seq.length xs - n]
-- Just i ->
-- -- really only need to filter where the next one after is False
--   let -- | In this case we have to stop one shy
--       wildRun = IS.fromList [0 .. i - n - 1]
--       fixedRun = IS.fromList [ i - n .. Seq.length xs - n ]
--       rest = Seq.drop i xs
--       -- 0123456789
--       -- 0000110000
--       --     123456
--       rests = take (Seq.length xs - i + 1) Seq.tails rest
--       -- the range exists, and either goes to the end of the seq or the next
--       -- one after is False
--   in  if

-- case Seq.elemIndexL False rest of
--       Nothing
--         | Seq.length rest > n -> IS.empty
--       -- 012345
--       -- 000011
--       --  ^^^
--       --  543
--         | otherwise -> IS.fromList [ i - n + Seq.length rest - 1 .. Seq.length xs - 1 ]
--       Just j
--         | j > n -> IS.empty
--       -- 0123456789
--       -- 0000110000
--       --  5432
--       --       2345
--       --     55555
--       --     4444
--       --     333
--       --     22
--       --  requirement is that the next one after must be False
--         | otherwise -> IS.fromList $
--             flip filter [ i - n .. Seq.length xs - n] \k ->
--               case Seq.lookup (k + n + 1) of
--                 Nothing -> _
--                 Just False -> True
--                 Just True -> False

-- where
--   wildRun = Seq.length $ Seq.takeWhileL not xs
--   wildElems = IS.fromList [ 0 .. wildRun - n - 1]
--   fixedRun = Seq.length $ Seq.takeWhileL id (Seq.drop wildRun xs)
--   fixedElems
--     | fixedRun > n = IS.empty
--     | otherwise = IS.fromList [ 0 .. ]
-- case Seq.elemIndexL True xs of
-- Nothing -> IS.fromList [0 .. Seq.length xs - n]
-- Just i ->
-- let rest = Seq.drop i xs
--     restTrue = case Seq.elemIndexL
--  in IS.fromList [ 0 .. i - n - 1 ]

-- sizeUntil :: a -> Seq a -> Int
-- sizeUntil x

-- <> case Seq.elemIndexL False rest of
--      Nothing -> undefined
--      Just j
--        | j <= n -> _
--        | otherwise -> _
-- where
--   firstTrue = Seq.elemIndexL

-- \| n <= 0 = case xs of
--     True :<| _ -> IS.empty
--     _ -> IS.singleton 0
-- \| otherwise = case xs of
--     Seq.Empty -> IS.empty
--     False :<| ys -> eater (n - 1) ys <> IS.mapMonotonic (+1) (eater n ys)
--     True :<| ys -> eater (n - 1) ys

-- | Ways the seq could be eaten by the run.  If False, the seq was eaten up
-- but the run was not used (ie, a string of wildcards)
eatState ::
  -- | positive
  Int ->
  StateT [NESeq Bool] [] Bool
eatState n = StateT \case
  [] -> [(False, [])]
  x : xs ->
    let skipBranch = do
          guard (not (or x)) $> (False, xs)
        eatBranch = do
          let x' = NESeq.toSeq x
          i <- eater n x'
          pure
            ( True,
              case NESeq.nonEmptySeq (Seq.drop (i + n + 1) x') of
                Nothing -> xs
                Just x'' -> x'' : xs
            )
     in eatBranch <|> skipBranch

-- ????????#??? 2,3

newtype StateMultiplicity s a = StateMultiplicity
  { runStateMultiplicity :: s -> Map (a, s) Int
  }

smGuard :: Bool -> StateMultiplicity s ()
smGuard False = StateMultiplicity $ const M.empty
smGuard True = StateMultiplicity $ \s -> M.singleton ((), s) 1

smGets :: (s -> a) -> StateMultiplicity s a
smGets f = StateMultiplicity \x -> M.singleton (f x, x) 1

infixl 1 `smBind`

smBind :: (Ord s, Ord b) => StateMultiplicity s a -> (a -> StateMultiplicity s b) -> StateMultiplicity s b
smBind (StateMultiplicity x) f = StateMultiplicity \s ->
  M.fromListWith
    (+)
    [ ((z, s''), i * j)
      | ((y, s'), i) <- M.toList $ x s,
        ((z, s''), j) <- M.toList $ runStateMultiplicity (f y) s'
    ]

-- newtype Multiplicity a = Multiplicity { runMultiplicity :: Map a Int }

-- | Ways the seq could be eaten by the run.  If False, the seq was eaten up
-- but the run was not used (ie, a string of wildcards)
eatState2 ::
  -- | positive
  Int ->
  StateMultiplicity [NESeq Bool] Bool
eatState2 n = StateMultiplicity \case
  [] -> M.singleton (False, []) 1
  x : xs ->
    let skipBranch = M.fromList . map (,1) $ do
          guard (not (or x)) $> (False, xs)
        eatBranch =
          let x' = NESeq.toSeq x
           in M.fromListWith (+) . map (,1) $ do
                i <- eater n x'
                pure
                  ( True,
                    case NESeq.nonEmptySeq (Seq.drop (i + n + 1) x') of
                      Nothing -> xs
                      Just x'' -> x'' : xs
                  )
     in M.unionWith (+) eatBranch skipBranch

eatRuns2 ::
  [Int] ->
  [NESeq Bool] ->
  Int
eatRuns2 ns0 = sum . runStateMultiplicity (go ns0)
  where
    go :: [Int] -> StateMultiplicity [NESeq Bool] ()
    go = \case
      [] -> StateMultiplicity \s ->
        if all (all not) s
          then M.singleton ((), s) 1
          else M.empty
      n : ns ->
        smGets (not . null) `smBind` \hasMore ->
          smGuard hasMore `smBind` \_ ->
            eatState2 n `smBind` \case
              True -> go ns
              False -> go (n : ns)

-- eaten <- eatState n
-- let ns'
--       | eaten = ns
--       | otherwise = n : ns
-- go ns'

eatRuns ::
  [Int] ->
  [NESeq Bool] ->
  [()]
eatRuns ns0 = evalStateT (go ns0)
  where
    go :: [Int] -> StateT [NESeq Bool] [] ()
    go = \case
      [] -> do
        allFalse <- gets (all (all not))
        guard allFalse
      n : ns -> do
        guard =<< gets (not . null)
        eaten <- eatState n
        -- rest <- get
        let ns'
              | eaten = ns
              | otherwise = n : ns
        go ns'

newtype Multiplicity a = Multiplicity {runMultiplicity :: a -> Map a Int}

instance (Ord a) => Semigroup (Multiplicity a) where
  Multiplicity f <> Multiplicity g = Multiplicity \x ->
    M.fromListWith
      (+)
      [ (z, i * j)
        | (y, i) <- M.toList $ g x,
          (z, j) <- M.toList $ f y
      ]

instance (Ord a) => Monoid (Multiplicity a) where
  mempty = Multiplicity (`M.singleton` 1)

mPlus :: (Ord a) => Multiplicity a -> Multiplicity a -> Multiplicity a
mPlus (Multiplicity f) (Multiplicity g) = Multiplicity \x ->
  M.unionWith (+) (f x) (g x)

traceSize :: Multiplicity a -> Multiplicity a
traceSize (Multiplicity f) = Multiplicity ((\q -> traceShow (M.elems q) q). f)

eatState3 ::
  -- | positive
  Int ->
  Multiplicity [NESeq Bool]
eatState3 n = Multiplicity \case
  -- [] -> M.singleton [] 1
  ~(x : xs) ->
    let x' = NESeq.toSeq x
     in M.fromListWith (+) . map (,1) $ do
          i <- eater n x'
          pure $ case NESeq.nonEmptySeq (Seq.drop (i + n + 1) x') of
            Nothing -> xs
            Just x'' -> x'' : xs

eatRuns3 ::
  [Int] ->
  [NESeq Bool] ->
  Int
eatRuns3 ns0 = sum . runMultiplicity (go ns0)
  where
    go :: [Int] -> Multiplicity [NESeq Bool]
    go = \case
      [] -> Multiplicity \s ->
        if all (all not) s
          then M.singleton s 1
          else M.empty
      n : ns -> Multiplicity \case
        [] -> M.empty
        x:xs
          | all not x -> M.unionWith (+)
                  (runMultiplicity (go ns <> eatState3 n) (x:xs))
                  (runMultiplicity (go (n:ns)) xs)
          | otherwise -> runMultiplicity (go ns <> eatState3 n) (x:xs)

        -- if not (null s)
        --    then runMultiplicity
        --           ( (go ns <> eatState3 n)
        --               `mPlus` Multiplicity \case
        --                 [] -> M.empty
        --                 x : xs
        --                   | all not x -> runMultiplicity (go (n : ns)) xs
        --                   | otherwise -> M.empty
        --           ) s
        --    else M.empty
        -- -- traceSize $
        -- --   <> Multiplicity \x -> if not (null x) then M.singleton x 1 else M.empty

-- smGets (not . null) `smBind` \hasMore ->
--   smGuard hasMore `smBind` \_ ->
--     eatState2 n `smBind` \case
--       True -> go ns
--       False -> go (n : ns)

day12b :: _ :~> _
day12b =
  MkSol
    { sParse = sParse day12a,
      sShow = show,
      sSolve =
        fmap (fmap sum) . traverse $ \(xs :: [Maybe Bool], pat :: [Int]) ->
          -- fmap sum . map $ \(xs :: [Maybe Bool], pat :: [Int]) ->
          let pat' :: [Int]
              pat' = concat $ replicate 5 pat
              xs' :: [Maybe Bool]
              xs' = intercalate [Nothing] $ replicate 5 xs
           in -- in traverse NESeq.nonEmptySeq (chunkUp xs)
              Just $ traceShowId $ solve4 xs' pat'
              -- in Just . length . eatRuns pat $ chunkUp xs
              -- in  runStateT (eatState (head pat)) <$> traverse NESeq.nonEmptySeq (chunkUp xs)
              -- in countTrue (matchesPat pat') $
              --      expandOut . map (\qs -> (length qs, head qs)) $
              --        group xs'
              -- countTrue ((== pat') . classify) $
              --        traverse (\case Nothing -> [False,True]; Just x -> [x]) xs'
    }
  where
    matchesPat pat = (== pat) . map fst . filter snd
