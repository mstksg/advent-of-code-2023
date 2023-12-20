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
  )
where

import AOC.Prelude
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
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tuple.Strict (T2 (..))
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

data ModuleType = FlipFlop | Conjuction
  deriving stock (Show, Eq, Ord, Generic)

instance NFData ModuleType

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
  { mdForward :: Set String,
    mdBackward :: Set String,
    mdType :: ModuleType
  }
  deriving stock (Show, Generic)

instance NFData ModuleData

data ModuleState = MS
  { msFlipFlops :: !(Set String),
    msConjunctions :: !(Map String (Set String))
  }
  deriving stock (Show, Generic)

instance NFData ModuleState

data ModuleConfig = MC
  { mcBroadcast :: Set String,
    mcModules :: Map String ModuleData
  }
  deriving stock (Show, Generic)

instance NFData ModuleConfig

assembleConfig :: [(Maybe (ModuleType, String), [String])] -> Maybe ModuleConfig
assembleConfig ms = do
  First bc <- broadcast
  pure $
    MC
      { mcBroadcast = bc,
        mcModules = M.intersectionWith (\(f,t) b -> MD f b t) forwards bckwrds
      }
  where
    T2 broadcast forwards =
      flip foldMap ms $ \(mn, ds) ->
        case mn of
          Just (a, n) -> T2 mempty (M.singleton n (S.fromList ds, a))
          Nothing -> T2 (Just (First (S.fromList ds))) mempty
    bckwrds =
      M.fromListWith
        (<>)
        [ (v, S.singleton k)
          | (k, (vs, _)) <- M.toList forwards,
            v <- S.toList vs
        ]

day20a :: _ :~> _
day20a =
  MkSol
    { sParse = traverse parseLine . lines,
      sShow = show,
      sSolve = assembleConfig
        -- noFail $
        --   id
    }

day20b :: _ :~> _
day20b =
  MkSol
    { sParse = sParse day20a,
      sShow = show,
      sSolve =
        noFail $
          id
    }
