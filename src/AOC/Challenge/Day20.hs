{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day20
  ( day20a,
    day20b,
    ModuleType (..),
    ModuleConfig (..),
    assembleConfig,
    firstEmissions,
    conjunctionTree,
    Tree (..),
    flatTree,
    defParens,
  )
where

import AOC.Prelude
import Data.Bitraversable
import qualified Data.Conduino as C
import qualified Data.Conduino.Combinators as C
import qualified Data.Conduino.Lift as C
import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
import Data.Functor.Foldable
import Data.Generics.Labels ()
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
import Data.Tuple.Strict (T2 (..))
import qualified Data.Tuple.Strict as STup
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

data ModuleType = FlipFlop | Conjuction
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

parseLine :: String -> Maybe (Maybe (ModuleType, String), [String])
parseLine = bitraverse pName (pure . pDest) <=< listTup . splitOn " -> "
  where
    pName = \case
      '%' : xs -> Just . Just $ (FlipFlop, xs)
      '&' : xs -> Just . Just $ (Conjuction, xs)
      "broadcaster" -> Just Nothing
      _ -> Nothing
    pDest = splitOn ", "

data ModuleData = MD
  { mdForward :: Seq String,
    mdBackward :: Set String,
    mdType :: ModuleType
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data PulseType = Low | High
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

data Pulse = Pulse {pOrigin :: !(Maybe String), pDest :: !String, pType :: !PulseType}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

data ModuleState = MS
  { msFlipFlops :: !(Set String),
    msConjunctions :: !(Map String (Set String))
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ModuleConfig = MC
  { mcBroadcast :: Seq String,
    mcModules :: Map String ModuleData
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

assembleConfig :: [(Maybe (ModuleType, String), [String])] -> Maybe ModuleConfig
assembleConfig ms = do
  First bc <- broadcast
  pure $
    MC
      { mcBroadcast = bc,
        mcModules =
          M.intersectionWith (\(f, t) b -> MD f b t) forwards bckwrds
            <> ((\(f, t) -> MD f S.empty t) <$> M.difference forwards bckwrds)
            <> ((\b -> MD Seq.empty b Conjuction) <$> M.difference bckwrds forwards)
      }
  where
    T2 broadcast forwards =
      flip foldMap ms $ \(mn, ds) ->
        case mn of
          Just (a, n) -> T2 mempty (M.singleton n (Seq.fromList ds, a))
          Nothing -> T2 (Just (First (Seq.fromList ds))) mempty
    bckwrds =
      M.fromListWith
        (<>)
        [ (v, S.singleton k)
          | (k, (vs, _)) <- M.toList forwards,
            v <- toList vs
        ]

stepPulse :: (MonadState ModuleState m) => ModuleConfig -> C.Pipe Pulse Pulse () m ()
stepPulse MC {..} = C.awaitForever \Pulse {..} -> case M.lookup pDest mcModules of
  Nothing -> pure ()
  Just MD {..} -> do
    newPulse <- case mdType of
      FlipFlop -> case pType of
        High -> pure Nothing
        Low ->
          fmap Just $
            #msFlipFlops . contains pDest %%= \wasOn ->
              if wasOn
                then (Low, False)
                else (High, True)
      Conjuction ->
        Just
          <$> uses (#msConjunctions . at pDest . non S.empty) \allHighs ->
            if allHighs == mdBackward
              then Low
              else High
    case newPulse of
      Just p -> do
        -- we really only need to update if they are conjunctions
        let updates = M.fromList $ map (,S.singleton pDest) (toList mdForward)
        modifying #msConjunctions \s ->
          M.unionWith
            (case p of Low -> S.difference; High -> S.union)
            s
            updates
        for_ mdForward \q -> C.yield $ Pulse (Just pDest) q p
      Nothing -> pure ()

pushButton :: (MonadState ModuleState m) => ModuleConfig -> C.Pipe i Pulse () m ()
pushButton mc@MC {..} =
  C.feedbackPipeEither $
    C.concatMap button
      C..| stepPulse mc
  where
    button = \case
      Left _ -> (\x -> Pulse Nothing x Low) <$> toList mcBroadcast
      Right x -> [x]

runModules :: Int -> ModuleConfig -> Int
runModules n0 mc@MC {..} = C.runPipePure p
  where
    p :: (Monad m) => C.Pipe i o u m Int
    p =
      C.replicate n0 ()
        C..| C.runStateP (MS S.empty M.empty) (pushButton mc)
        C..| (multTypes <$> C.foldMap countTypes)
    countTypes :: Pulse -> STup.T2 (Sum Int) (Sum Int)
    countTypes Pulse {..} = case pType of
      Low -> STup.T2 1 0
      High -> STup.T2 0 1
    multTypes :: STup.T2 (Sum Int) (Sum Int) -> Int
    multTypes (STup.T2 (Sum x) (Sum y)) = (x + n0 * (1 + Seq.length mcBroadcast)) * y

data Tree k = TNode k [Tree k]
  deriving stock (Functor, Eq, Ord, Show)

makeBaseFunctor ''Tree

conjunctionTree :: ModuleConfig -> String -> TreeF String String
conjunctionTree MC {..} = go
  where
    go k = TNodeF k case M.lookup k mcModules of
      Nothing -> []
      Just md -> case mdType md of
        FlipFlop -> []
        Conjuction -> filter (\b -> mdType (mcModules M.! b) == Conjuction) . toList $ mdBackward md

defParens :: [(Char, Char)]
defParens = cycle [('(', ')'), ('{', '}'), ('[', ']')]

flatTree :: (a -> Char) -> TreeF a ([(Char, Char)] -> String -> String) -> [(Char, Char)] -> String -> String
flatTree f = go
  where
    go (TNodeF x xs) ((a, b) : ps) = (f x :) . childs
      where
        childs
          | null xs = id
          | otherwise = (a :) . foldr (.) id (map ($ ps) xs) . (b :)

-- f x <> b <> concatMap ($ bs) xs
-- (f x <>) . foldr (.) id xs
-- f x <> sep <>

firstEmissions :: ModuleConfig -> IO ()
firstEmissions mc@MC {..} = C.runPipe p
  where
    p :: C.Pipe i o u IO ()
    p =
      C.evalStateP (MS S.empty M.empty) $
        C.iterate (+ 1) (0 :: Int)
          C..| C.passthrough
            ( pushButton mc
                C..| C.filter (any (`S.member` conjunctions) . pOrigin)
                C..| C.mapM displayMap
                C..| C.mapAccum changes Nothing
                C..| C.concat
                -- C..| C.mapAccum flips M.empty
                -- C..| C.concat
            )
          C..| C.iterM (\(Just i, str) -> liftIO $ printf "% 9d: %s\n" i str)
          -- C..| C.iterM (liftIO . putStrLn <=< display)
          C..| C.sinkNull
    displayMap Pulse{..} = do
      conjState <- use #msConjunctions
      pure $ cata
              ( flatTree
                  ( \n ->
                      let allHighs = M.findWithDefault S.empty n conjState
                          isAllOn = allHighs == mdBackward (mcModules M.! n)
                          isNew = False
                          -- isNew = Just n == pOrigin
                       in case (isAllOn, isNew) of
                            (True, True) -> '▓'
                            (True, False) -> '▒'
                            (False, True) -> '.'
                            (False, False) -> ' '
                  )
              )
              cTree
              defParens
              ""
    -- display (Just i, Pulse {..}) = do
    --   conjState <- use #msConjunctions
    --   let outMap =
    --         cata
    --           ( flatTree
    --               ( \n ->
    --                   let allHighs = M.findWithDefault S.empty n conjState
    --                       isAllOn = allHighs == mdBackward (mcModules M.! n)
    --                       isNew = Just n == pOrigin
    --                    in case (isAllOn, isNew) of
    --                         (True, True) -> '▓'
    --                         (True, False) -> '▒'
    --                         (False, True) -> '.'
    --                         (False, False) -> ' '
    --               )
    --           )
    --           cTree
    --           defParens
    --           ""
    --   pure $ printf "% 9d: %s" i outMap

    -- -- let outMap = flip M.fromSet conjunctions \n ->
    -- --       let allHighs = M.findWithDefault S.empty n conjState
    -- --           isAllOn = allHighs == mdBackward (mcModules M.! n)
    -- --           isNew = Just n == pOrigin
    -- --        in case (isAllOn, isNew) of
    -- --             (True, True) -> '▓'
    -- --             (True, False) -> '▒'
    -- --             (False, True) -> '.'
    -- --             (False, False) -> ' '
    -- -- pure $ printf "% 7d: %s (%s)" i (M.elems outMap) (concat pOrigin)
    -- display _ = undefined
    -- uses #msConjunctions $ _
    -- printf "%d: %s %s (%s)" i (show pType) pDest (show $ mdBackward $ mcModules M.! pDest)
    -- display (Just i, Pulse{..}) = printf "%d: %s %s (%s)" i (show pType) pDest (show $ mdBackward $ mcModules M.! pDest)
    -- emitUnique :: String -> Set String -> (Set String, Maybe String)
    -- emitUnique dest !seen
    --   | dest `S.notMember` seen = (S.insert dest seen, Just dest)
    --   | otherwise = (seen, Nothing)
    cTree :: Tree String
    cTree = ana (conjunctionTree mc) "rx"
    conjunctions :: Set String
    conjunctions = M.keysSet $ M.filter ((== Conjuction) . mdType) mcModules
    flips :: Pulse -> Map (Maybe String) PulseType -> (Map (Maybe String) PulseType, Maybe Pulse)
    flips pulse@Pulse {..} hist = case M.lookup pOrigin hist of
      Nothing -> (M.insert pOrigin pType hist, Just pulse)
      Just oldType
        | oldType == pType -> (hist, Nothing)
        | otherwise -> (M.insert pOrigin pType hist, Just pulse)
    changes a = \case
      Nothing -> (Just a, Just a)
      Just b
        | a == b -> (Just b, Nothing)
        | otherwise -> (Just a, Just a)


-- fromMaybe (hist, Nothing) do
-- case M.lookup pDest hist of

-- lowConj :: Pulse -> Maybe String
-- lowConj Pulse{..} = case pType of
--   Low ->

-- firstEmissions :: ModuleConfig -> Map String Int
-- firstEmissions mc = evalState (C.runPipe p) (MS S.empty M.empty)
--   where
--     p :: C.Pipe i o u (State ModuleState) Int
--     p =
--       C.replicate n0 ()
--         C..| pushButton mc
--         C..| (multTypes <$> C.foldMap countTypes)

-- evalState (go n0 0 0) (MS S.empty M.empty)
-- where
--   go :: Int -> Int -> Int -> State ModuleState Int
--   go n numLow numHigh
--     | n <= 0 = pure $ numLow * numHigh
--   go n !numLow !numHigh = do
--     (newLow, newHigh) <- pushButton mc
--     traceM =<<  gets (latchStates mc)
--     go (n - 1) (numLow + newLow) (numHigh + newHigh)

-- where
--   go !numLow !numHigh = \case
--     Nothing ->
--       go (numLow + 1) numHigh (Just ((,Low) <$> mcBroadcast))
--     Just Seq.Empty -> pure (numLow, numHigh)
--     Just ((dest, pulseType) Seq.:<| queue) -> do
--       queue' <- stepPulse mc dest pulseType
--       let (numLow', numHigh') = case pulseType of
--             Low -> (numLow + 1, numHigh)
--             High -> (numLow, numHigh + 1)
--       go numLow' numHigh' (Just (queue <> queue'))

-- stepPulse :: Monad m => ModuleConfig -> Pulse -> StateT ModuleState m (Seq Pulse)
-- stepPulse MC {..} Pulse{..} = case M.lookup pDest mcModules of
--   Nothing -> pure Seq.empty
--   Just MD {..} -> do
--     newPulse <- case mdType of
--       FlipFlop -> case pType of
--         High -> pure Nothing
--         Low ->
--           fmap Just $
--             #msFlipFlops . contains pDest %%= \wasOn ->
--               if wasOn
--                 then (Low, False)
--                 else (High, True)
--       Conjuction ->
--         Just
--           <$> uses (#msConjunctions . at pDest . non S.empty) \allHighs ->
--             if allHighs == mdBackward
--               then Low
--               else High
--     case newPulse of
--       Just p -> do
--         -- we really only need to update if they are conjunctions
--         let updates = M.fromList $ map (,S.singleton pDest) (toList mdForward)
--         modifying #msConjunctions \s ->
--           M.unionWith
--             (case p of Low -> S.difference; High -> S.union)
--             s
--             updates
--         pure $ (`Pulse` p) <$> mdForward
--       Nothing -> pure Seq.empty

-- pushButton :: ModuleConfig -> State ModuleState (Int, Int)
-- pushButton mc@MC {..} = go 0 0 Nothing
--   where
--     go !numLow !numHigh = \case
--       Nothing ->
--         go (numLow + 1) numHigh (Just ((,Low) <$> mcBroadcast))
--       Just Seq.Empty -> pure (numLow, numHigh)
--       Just ((dest, pulseType) Seq.:<| queue) -> do
--         queue' <- stepPulse mc dest pulseType
--         let (numLow', numHigh') = case pulseType of
--               Low -> (numLow + 1, numHigh)
--               High -> (numLow, numHigh + 1)
--         go numLow' numHigh' (Just (queue <> queue'))

-- latchStates :: ModuleConfig -> ModuleState -> String
-- latchStates MC{..} MS{..} =
--     [ if isReady then '#' else '.'
--       | (n, MD{ mdType = Conjuction, ..}) <- M.toList mcModules
--     , let allHighs = M.findWithDefault S.empty n msConjunctions
--           isReady = allHighs == mdBackward
--     ]

-- runModules :: Int -> ModuleConfig -> Int
-- runModules n0 mc = evalState (go n0 0 0) (MS S.empty M.empty)
--   where
--     go :: Int -> Int -> Int -> State ModuleState Int
--     go n numLow numHigh
--       | n <= 0 = pure $ numLow * numHigh
--     go n !numLow !numHigh = do
--       (newLow, newHigh) <- pushButton mc
--       traceM =<<  gets (latchStates mc)
--       go (n - 1) (numLow + newLow) (numHigh + newHigh)

-- pushButton2 :: ModuleConfig -> State ModuleState (Set String)
-- pushButton2 mc@MC {..} = go S.empty Nothing
--   where
--     go emitted  = \case
--       Nothing ->
--         go emitted (Just ((,Low) <$> mcBroadcast))
--       Just Seq.Empty -> emitted
--       Just ((dest, pulseType) Seq.:<| queue) -> do
--         -- let emitted' = case M.lookup dest mcModules of
--         --       Just MD{..}
--         --         | mdType == Conjuction && pulseType == High puy
--         queue' <- stepPulse mc dest pulseType
--         let (numLow', numHigh') = case pulseType of
--               Low -> (numLow + 1, numHigh)
--               High -> (numLow, numHigh + 1)
--         go emitted' (Just (queue <> queue'))

-- runModules2 :: ModuleConfig -> Int
-- runModules2 mc = evalState (go 1) (MS S.empty M.empty)
--   where
--     go :: Int -> State ModuleState Int
--     go !n = do
--       rxHit <- pushButton2 mc
--       if rxHit
--         then pure n
--         else go (n + 1)

day20a :: _ :~> _
day20a =
  MkSol
    { sParse = traverse parseLine . lines,
      sShow = show,
      sSolve = fmap (runModules 1000) . assembleConfig
      -- sSolve = fmap (runModules 1000) . assembleConfig
    }

day20b :: _ :~> _
day20b =
  MkSol
    { sParse = sParse day20a,
      sShow = show,
      sSolve = Just
      -- sSolve = fmap (runModules 1000000) . assembleConfig
    }
