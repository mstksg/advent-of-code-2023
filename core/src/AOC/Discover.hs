{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : AOC.Discover
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Template Haskell for discovering all named challenges in a given
-- directory.
--

module AOC.Discover (
    mkChallengeMap
  , solutionList
  , ChallengeMap
  , ChallengeSpec(..)
  , lookupSolution
  , solSpec
  , solSpecStr
  , solSpecStr_
  , charPart
  ) where

import           AOC.Solver
import           Advent
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor
import           Data.Function
import           Data.Map                               (Map)
import           Data.Maybe
import           Data.Traversable
import           Data.Void
import           GHC.Exts
import           Language.Haskell.TH                    as TH
import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Syntax             (TExp(..))
import           Prelude
import           Text.Printf
import           Data.Set                               (Set)
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Map                               as M
import qualified Data.Set as S
import qualified Text.Megaparsec                        as P
import qualified Text.Megaparsec.Char                   as P
import qualified Text.Megaparsec.Char.Lexer             as PL

-- | Big quick escape hatch if things explode in the middle of solving.
-- This will disable the check for NFData when using 'MkSomeSol' and assume
-- no NFData in every case.
checkIfNFData :: Bool
checkIfNFData = True
-- checkIfNFData = False

-- | A specification for a specific challenge.  Should consist of a day and
-- a lowercase character.
data ChallengeSpec = CS { _csYear :: Integer
                        , _csDay  :: Day
                        , _csPart :: Part
                        }
  deriving stock (Show, Eq, Ord)

-- | The magic: implementers can register solutions.
class HasSolution (cs :: ChallengeSpec) where
    -- | Avoid using singletons
    autoChallengeSpec :: ChallengeSpec
    autoSolution :: SomeSolution

-- | A map of days to parts to solutions.
type ChallengeMap = Map Integer (Map Day (Map Part SomeSolution))

-- | Lookup up a solution from a 'ChallengeMap'
lookupSolution :: ChallengeSpec -> Map Integer (Map Day (Map Part a)) -> Maybe a
lookupSolution CS{..} = M.lookup _csPart <=< M.lookup _csDay <=< M.lookup _csYear

-- | Get a 'ChallengeSpec' from a given reified solution (name).
--
-- @
-- solSpec \'day02a == CS { _csDay = 1, _csPart = 'a' }
-- @
--
solSpec :: Integer -> TH.Name -> ChallengeSpec
solSpec y = solSpecStr_ y . nameBase

solSpecStr :: Integer -> String -> Either (P.ParseErrorBundle String Void) ChallengeSpec
solSpecStr y = P.runParser (challengeName y) ""

solSpecStr_ :: Integer -> String -> ChallengeSpec
solSpecStr_ y = either (error . P.errorBundlePretty) id . solSpecStr y

type Parser = P.Parsec Void String

-- | Template Haskell splice to produce a list of all named solutions in
-- scope. Expects solutions as function names following the format
-- @dayDDp@, where @DD@ is a two-digit zero-added day, and @p@ is
-- a lower-case letter corresponding to the part of the challenge.
--
-- See 'mkChallengeMap' for a description of usage.
solutionList :: Integer -> Code Q [(Day, (Part, SomeSolution))]
solutionList y = Code $
        fmap (TExp . ListE . catMaybes)
      $ traverse (fmap (fmap unType) . specExp) (S.toList (challengeSpecUniverse y))

-- | Meant to be called like:
--
-- @
-- mkChallengeMap $$(solutionList)
-- @
mkChallengeMap :: Integer -> [(Day, (Part, SomeSolution))] -> ChallengeMap
mkChallengeMap y = M.singleton y
                 . M.unionsWith M.union
                 . map (uncurry M.singleton . second (uncurry M.singleton))

challengeSpecUniverse :: Integer -> Set ChallengeSpec
challengeSpecUniverse y = S.delete (CS y (mkDay_ 25) Part2) . S.fromList $
    CS y <$> [minBound .. maxBound] <*> [minBound .. maxBound]

-- | Looks up the name in scope
specExp :: ChallengeSpec -> Q (Maybe (TExp (Day, (Part, SomeSolution))))
specExp s@(CS _ d p) = do
    mn <- lookupValueName (specName s)
    for mn \n -> do
      isNF <- solverNFData n
      let con
            | isNF = 'MkSomeSolNF
            | otherwise = 'MkSomeSolWH
      pure $ TExp $ tTupE
        [ VarE 'mkDay_ `AppE` LitE (IntegerL (dayInt d))
        , tTupE
            [ ConE (partCon p)
            , ConE con `AppE` VarE (mkName (specName s))
            ]
        ]
  where
    partCon Part1 = 'Part1
    partCon Part2 = 'Part2
    tTupE = TupE . fmap Just

specName :: ChallengeSpec -> String
specName (CS _ d p) = printf "day%02d%c" (dayInt d) (partChar p)

challengeName :: Integer -> Parser ChallengeSpec
challengeName y = do
    _    <- P.string "day"
    dInt <- PL.decimal
    dFin <- maybe (fail $ "Day not in range: " ++ show dInt) pure $
                mkDay dInt
    c    <- P.lowerChar
    p    <- maybe (fail $ printf "Part not parsed: %c" c) pure $
                charPart c
    pure $ CS y dFin p

-- | Parse a 'Char' into a 'Part'
charPart :: Char -> Maybe Part
charPart 'a' = Just Part1
charPart 'b' = Just Part2
charPart _   = Nothing

-- | Check if a solver identifier is of type @A ':~>' B@, where @B@ is an
-- instance of 'NFData'.
solverNFData :: TH.Name -> Q Bool
solverNFData n
  | checkIfNFData = reify n >>= \case
      VarI _ (ConT c `AppT` a `AppT` _) _
        | c == ''(:~>) -> deepInstance ''NFData a
      _ -> pure False
  | otherwise     = pure False

-- | Check if a type is an instance of a class, unifying when possible
deepInstance
    :: TH.Name  -- ^ class
    -> TH.Type  -- ^ type
    -> Q Bool
deepInstance cn = fmap isJust . runMaybeT . deepInstance_ cn

deepInstance_
    :: TH.Name  -- ^ class
    -> TH.Type  -- ^ type
    -> MaybeT Q ()
deepInstance_ cn t = do
    insts <- maybe empty pure . NE.nonEmpty =<< lift (reifyInstances cn [t])
    forM_ insts $ \case
      InstanceD _ ctx instHead _ -> do
        uni <- lift $ unifyTypes [ConT cn `AppT` t, instHead]
        forM_ ctx $ \case
          AppT (ConT c) v -> deepInstance_ c (applySubstitution uni v)
          _               -> empty
      _                            -> empty
