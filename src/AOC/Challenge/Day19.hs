{-# OPTIONS_GHC -Wno-unused-imports   #-}
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

module AOC.Challenge.Day19 (
    day19a
  , day19b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

-- qs{s>3448:A,lnx}

data XMAS = X | M | A | S
  deriving (Eq, Ord, Show, Generic)

instance NFData XMAS

chunky :: String -> (Maybe String, [Either String (XMAS,Ordering,Int,Maybe String)])
chunky str = (mfilter (not . null) (Just inp), map go chunks)
  where
    (inp, str') = span (/='{') str
    chunks = splitOn "," $ filter (`notElem` "{}") str'
    go x = maybe (Left x) Right do
      a:(o:x') <- pure x
      a' <- case a of
        'x' -> Just X
        'm' -> Just M
        'a' -> Just A
        's' -> Just S
        _ -> Nothing
      o' <- case o of
        '<' -> Just LT
        '>' -> Just GT
        '=' -> Just EQ
        _ -> Nothing
      (b,x'') <- pure $ span isDigit x'
      b' <- readMaybe b
      pure (a', o', b', tailMay x'')

processFilter :: String -> Maybe (String, ([(XMAS, Ordering, Int, Either Bool String)], Either Bool String))
processFilter str = do
  (Just x, parts) <- pure $ chunky str
  filts <- for (init parts) \case
    Right (a,b,c,d) -> (a,b,c,) . classify <$> d
    Left _ -> Nothing
  Left bu <- pure $ last parts
  pure (x, (filts, classify bu))
  where
    classify = \case
      "R" -> Left False
      "A" -> Left True
      p -> Right p

processInp :: String -> Maybe (Map XMAS Int)
processInp str = M.fromList <$> for parts (\case Right (x,_,n,_) -> Just (x, n); _ -> Nothing)
  where
    (_, parts) = chunky str


day19a :: _ :~> _
day19a = MkSol
    { sParse = \inp -> case splitOn "\n\n" inp of
                         [a,b] -> (,)  <$> fmap M.fromList (traverse processFilter (lines a))
                                       <*> traverse processInp (lines b)
                         _ -> Nothing
    , sShow  = show
    , sSolve = \(filts, xs) -> Just . sum . map sum . filter (accepted filts) $ xs
    }
  where
    accepted filts mp = go "in"
      where
        go i = case determine (filts M.! i) of
          Right j -> go j
          Left b -> b
        determine (fs, x) = foldr go' x fs
          where
            go' (y, o, n, next) rest
                | compare (mp M.! y) n == o = next
                | otherwise = rest

day19b :: _ :~> _
day19b = MkSol
    { sParse = sParse day19a
    , sShow  = show
    , sSolve = noFail $
          id
    }
