{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day18 (
    day18a
  , day18b
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
import Data.Bitraversable
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Numeric.Lens (hex)

-- R 5 (#4f4602)

parseLineA :: String -> Maybe (Dir, Int)
parseLineA = bitraverse (parseDir . head) readMaybe
        <=< listTup . take 2 . words

shoelace :: NonEmpty Point -> Int
-- shoelace (z:|zs) = (`div` 2) . abs . sum $ traceShow (zip (z:zs) (zs)) $ zipWith go (z:zs) (zs ++ [z])
shoelace (z:|zs) = (`div` 2) . abs . sum $ zipWith go (z:zs) (zs ++ [z])
  where
    latterPart = zs ++ [z]
    go (V2 x y) (V2 x' y') = x * y' - y * x'

day18a :: _ :~> _
day18a = MkSol
    { sParse = traverse parseLineA . lines
    , sShow  = show
    , sSolve = fmap shoelace . NE.nonEmpty . scanl' go 0
    }
  where
    go p (d, i) = p + i *^ dirPoint' d

parseLineB :: String -> Maybe (Dir, Int)
parseLineB = splitUp . filter isHexDigit <=< (!? 2) . words
  where
    splitUp xs = do
      y:ys <- pure $ reverse xs
      d <- case y of
        '0' -> pure East
        '1' -> pure South
        '2' -> pure West
        '3' -> pure North
        _ -> empty
      i <- preview (reversed . hex) ys
      pure (d, i)

day18b :: _ :~> _
day18b = MkSol
    { sParse = traverse parseLineB . lines
    , sShow  = show
    , sSolve = fmap shoelace . NE.nonEmpty . scanl' go 0
    }
  where
    go p (d, i) = p + i *^ dirPoint d
