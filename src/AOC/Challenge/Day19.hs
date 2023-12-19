{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day19
  ( day19a,
    day19b,
  )
where

import AOC.Prelude
import Data.Functor.Foldable
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Interval (Interval)
import qualified Data.Interval as IV
import Data.IntervalMap.Strict (IntervalMap)
import qualified Data.IntervalMap.Strict as IVM
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IVS
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

-- qs{s>3448:A,lnx}

data XMAS = X | M | A | S
  deriving stock (Eq, Ord, Show, Generic, Enum)

instance NFData XMAS

xmasMap :: Map Char XMAS
xmasMap = M.fromList $ zip "xmas" [X .. S]

opMap :: Map Char Ordering
opMap = M.fromList $ zip "<=>" [LT, EQ, GT]

data Result a = Reject | Accept | Defer a
  deriving stock (Eq, Ord, Show, Generic, Functor)

instance (NFData a) => NFData (Result a)

result :: b -> b -> (a -> b) -> Result a -> b
result r a d = \case
  Reject -> r
  Accept -> a
  Defer x -> d x

-- | like Either monad
instance Applicative Result where
  pure = Defer
  (<*>) = \case
    Reject -> const Reject
    Accept -> const Accept
    Defer f -> \case
      Reject -> Reject
      Accept -> Accept
      Defer x -> Defer (f x)

instance Monad Result where
  (>>=) = \case
    Reject -> const Reject
    Accept -> const Accept
    Defer x -> ($ x)

data Rule a = Rule
  { rXmas :: XMAS,
    rOp :: Ordering,
    rVal :: Int,
    rResult :: Result a
  }
  deriving stock (Eq, Ord, Show, Generic, Functor)

instance (NFData a) => NFData (Rule a)

data Workflow a = Workflow
  { wfRules :: [Rule a],
    wfDefault :: Result a
  }
  deriving stock (Eq, Ord, Show, Generic, Functor)

instance (NFData a) => NFData (Workflow a)

chunky :: String -> (String, [Either String (XMAS, Ordering, Int, Maybe String)])
chunky str = (inp, map go chunks)
  where
    (inp, str') = span (/= '{') str
    chunks = splitOn "," $ filter (`notElem` "{}") str'
    go x = maybe (Left x) Right do
      a : o : x' <- pure x
      a' <- M.lookup a xmasMap
      o' <- M.lookup o opMap
      let (b, x'') = span isDigit x'
      b' <- readMaybe b
      pure (a', o', b', tailMay x'')

parseWorkflow :: String -> Maybe (String, Workflow String)
parseWorkflow str = do
  (conds, Left backup) <- unsnoc filterParts
  rules <- for conds \case
    Right (a, b, c, Just d) -> Just $ Rule a b c (classify d)
    _ -> Nothing
  pure (key, Workflow rules (classify backup))
  where
    (key, filterParts) = chunky str
    classify = \case
      "R" -> Reject
      "A" -> Accept
      p -> Defer p

parseBag :: String -> Maybe (Map XMAS Int)
parseBag = fmap M.fromList . traverse go . snd . chunky
  where
    go = \case
      Right (x, _, n, _) -> Just (x, n)
      _ -> Nothing

-- accepted ::
--   Map String (Workflow String) ->
--   Map XMAS Int ->
--   Bool
-- accepted filts mp = go "in"
--   where
--     go i = case determine (filts M.! i) of
--       Defer j -> go j
--       Accept -> True
--       Reject -> False
--     determine Workflow {..} = foldr go' wfDefault wfRules
--       where
--         go' Rule {..} rest
--           | compare (mp M.! rXmas) rVal == rOp = rResult
--           | otherwise = rest

evalWorkflow :: Map XMAS Int -> Workflow Bool -> Bool
evalWorkflow mp = go
  where
    go Workflow {..} = foldr eval (unResult wfDefault) wfRules
    eval Rule {..} rest
      | compare (mp M.! rXmas) rVal == rOp = unResult rResult
      | otherwise = rest
    unResult = result False True id

xmasRange :: Interval Int
xmasRange = IV.Finite 1 IV.<=..<= IV.Finite 4000

newtype XmasSet = XmasSet (IntervalMap Int (IntervalMap Int (IntervalMap Int (IntervalSet Int))))
  deriving stock (Eq, Show, Generic)

instance NFData XmasSet

allXmas :: XmasSet
allXmas =
  XmasSet
    . IVM.singleton xmasRange
    . IVM.singleton xmasRange
    . IVM.singleton xmasRange
    $ IVS.singleton xmasRange

noXmas :: XmasSet
noXmas = XmasSet IVM.empty

reMap :: (Ord k, Eq a) => IntervalMap k a -> IntervalMap k a
reMap = IVM.fromList . eat . IVM.toAscList
  where
    eat [] = []
    eat ((i, v) : xs) = go i v xs
    go i v = \case
      [] -> [(i, v)]
      (j, u) : xs
        | i `IV.isConnected` j && v == u -> go (i `IV.hull` j) v xs
        | otherwise -> (i, v) : go j u xs

intersect :: XmasSet -> XmasSet -> XmasSet
intersect (XmasSet xs) (XmasSet xs') =
  XmasSet $ subInter (subInter (subInter IVS.intersection)) xs xs'
  where
    subInter f x = reMap . IVM.intersectionWith f x

union :: XmasSet -> XmasSet -> XmasSet
union (XmasSet xs) (XmasSet xs') =
  XmasSet $ subUnion (subUnion (subUnion IVS.union)) xs xs'
  where
    subUnion f x = reMap . IVM.unionWith f x

-- ....x   .....   ....x
-- .x..x   x...x   .x...
-- .xx.x   xx.x.   ..x.x
-- x..x.   .xx.x   x..x.
--
-- combine A^B and whatever is in A but not B
difference :: XmasSet -> XmasSet -> XmasSet
difference (XmasSet xs) (XmasSet xs') =
  XmasSet $
    diffWith (diffWith (diffWith IVS.difference)) xs xs'
  where
    diffWith f a b = reMap $ IVM.intersectionWith f a b <> IVM.difference a b

size :: XmasSet -> Int
size (XmasSet xs) = (sumBySize . sumBySize . sumBySize) (sum . map ivalSize . IVS.toList) xs
  where
    sumBySize f = sum . map (\(i, a) -> ivalSize i * f a) . IVM.toList
    ivalSize i =
      IV.width i
        - 1
        + countTrue (== IV.Closed) (map snd [IV.lowerBound' i, IV.upperBound' i])

xmasRule :: Rule XmasSet -> XmasSet -> XmasSet
xmasRule Rule {..} rest = case rResult of
  Reject -> rest `difference` ivalXmas
  Accept -> ivalXmas `union` rest
  Defer s -> (s `intersect` ivalXmas) `union` (rest `difference` ivalXmas)
  where
    ival = case rOp of
      LT -> IV.Finite 1 IV.<=..< IV.Finite rVal
      GT -> IV.Finite rVal IV.<..<= IV.Finite 4000
      EQ -> IV.singleton rVal
    ivalXmas = case rXmas of
      X -> XmasSet $ IVM.singleton ival . IVM.singleton xmasRange . IVM.singleton xmasRange $ IVS.singleton xmasRange
      M -> XmasSet $ IVM.singleton xmasRange . IVM.singleton ival . IVM.singleton xmasRange $ IVS.singleton xmasRange
      A -> XmasSet $ IVM.singleton xmasRange . IVM.singleton xmasRange . IVM.singleton ival $ IVS.singleton xmasRange
      S -> XmasSet $ IVM.singleton xmasRange . IVM.singleton xmasRange . IVM.singleton xmasRange $ IVS.singleton ival

workflowInterval :: Workflow XmasSet -> XmasSet
workflowInterval Workflow {..} = foldr xmasRule (unResult wfDefault) wfRules
  where
    unResult = result noXmas allXmas id

day19a :: _ :~> _
day19a =
  MkSol
    { sParse = \inp -> do
        (a, b) <- listTup $ splitOn "\n\n" inp
        (,)
          <$> fmap M.fromList (traverse parseWorkflow (lines a))
          <*> traverse parseBag (lines b),
      sShow = show,
      sSolve = noFail $ \(wfs, xs) ->
        sum
          . map sum
          . filter (\x -> hylo (evalWorkflow x) (wfs M.!) "in")
          $ xs
    }

day19b :: _ :~> _
day19b =
  MkSol
    { sParse = fmap M.fromList . traverse parseWorkflow . takeWhile (not . null) . lines,
      sShow = show,
      sSolve =
        noFail $ \wfs ->
          size $ hylo workflowInterval (wfs M.!) "in"
    }
