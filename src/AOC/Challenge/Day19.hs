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
import Data.IntervalMap.Lazy (IntervalMap)
import qualified Data.IntervalMap.Lazy as IVM
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

processFilter :: String -> Maybe (String, Workflow String)
processFilter str = do
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

processInp :: String -> Maybe (Map XMAS Int)
processInp = fmap M.fromList . traverse go . snd . chunky
  where
    go = \case
      Right (x, _, n, _) -> Just (x, n)
      _ -> Nothing

accepted ::
  Map String (Workflow String) ->
  Map XMAS Int ->
  Bool
accepted filts mp = go "in"
  where
    go i = case determine (filts M.! i) of
      Defer j -> go j
      Accept -> True
      Reject -> False
    determine Workflow {..} = foldr go' wfDefault wfRules
      where
        go' Rule {..} rest
          | compare (mp M.! rXmas) rVal == rOp = rResult
          | otherwise = rest

accepted2 ::
  Map String (Workflow String) ->
  Map XMAS Int ->
  Bool
accepted2 filts mp =
  result False True absurd
    . hylo (cataWorkflow mp) (anaWorkflow filts)
    $ "in"

anaWorkflow :: Map String (Workflow String) -> String -> Workflow String
anaWorkflow wfs = (wfs M.!)

cataWorkflow :: Map XMAS Int -> Workflow (Result a) -> Result a
cataWorkflow mp = go
  where
    go Workflow {..} = foldr eval (join wfDefault) wfRules
    eval Rule {..} rest
      | compare (mp M.! rXmas) rVal == rOp = join rResult
      | otherwise = rest

newtype XmasSet = XmasSet (IntervalMap Int (IntervalMap Int (IntervalMap Int (IntervalSet Int))))

intersect :: XmasSet -> XmasSet -> XmasSet
intersect (XmasSet xs) (XmasSet xs') =
  XmasSet $
    IVM.intersectionWith
      (IVM.intersectionWith (IVM.intersectionWith IVS.intersection))
      xs
      xs'

union :: XmasSet -> XmasSet -> XmasSet
union (XmasSet xs) (XmasSet xs') =
  XmasSet $
    IVM.unionWith
      (IVM.unionWith (IVM.unionWith IVS.union))
      xs
      xs'

-- difference :: XmasSet -> XmasSet -> XmasSet
-- difference = 
--   XmasSet $
--     IVM.intersectionWith
--       (IVM.intersectionWith (IVM.intersectionWith IVS.intersection))
--       xs
--       xs'

-- cataWorkflow2 :: Workflow XmasSet -> XmasSet
-- cataWorkflow2 Workflow{..} = _


day19a :: _ :~> _
day19a =
  MkSol
    { sParse = \inp -> do
        (a, b) <- listTup $ splitOn "\n\n" inp
        (,)
          <$> fmap M.fromList (traverse processFilter (lines a))
          <*> traverse processInp (lines b),
      sShow = show,
      sSolve = \(filts, xs) -> Just . sum . map sum . filter (accepted2 filts) $ xs
    }

day19b :: _ :~> _
day19b =
  MkSol
    { sParse = sParse day19a,
      sShow = show,
      sSolve =
        noFail $
          id
    }
