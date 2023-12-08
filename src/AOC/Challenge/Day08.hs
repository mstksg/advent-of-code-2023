{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day08 (
    day08a
  , day08b
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

-- LLRRRLLRLRRRLLRLRLRLRLRRRLRRLRRLRLLLRRLLRRLRRLRRLRRRLLLRRLRLRRRLRRRLRLRRLRRRLRLRRRLRLRLLLRLRRLRLRRLRRRLRLRRRLRRRLRRRLRRRLRLRRRLRRRLRLLRRLRLRLRRRLRRLRRRLRRRLRRRLRRRLLLLRRLLRLRRLRRLRRRLRRRLLLRRLRRLRLRRLRRRLRRLRLRRRLRLRRLLRLLRRLRLRRRLRRLRRLRLRRLLLRRRLRLRRRLRLRLLRLRLRRRLRLRLRRRLRRLRRLRRRLRRLLRRRR

-- VTM = (VPB, NKT)
-- LHN = (DLF, GQV)

parseMe :: [String] -> ([Bool], Map String (String, String))
parseMe (x:_:xs) = (map (== 'R') x, M.fromList$
    [ (a, (filter isAlphaNum b, filter isAlphaNum c)) | [a,"=",b,c] <- words <$> xs]
    )

day08a :: _ :~> _
day08a = MkSol
    { sParse = noFail $
        parseMe . lines
    , sShow  = show
    , sSolve = noFail $ \(xs, mp) ->
        subtract 1 . length $ iterateMaybe (\(s,r:rs) ->
            guard (s /= "ZZZ") $>
                if r
                  then (snd $ mp M.! s, rs)
                  else (fst $ mp M.! s, rs)
            )
          ("AAA", cycle xs)
    }

day08b :: _ :~> _
day08b = MkSol
    { sParse = sParse day08a
    , sShow  = show
    , sSolve = noFail $ \(xs, mp) ->

        let cycleLengths = 
              [ subtract 1 . length $ iterateMaybe (\(s,r:rs) ->
                  guard (last s /= 'Z') $>
                      if r
                        then (snd $ mp M.! s, rs)
                        else (fst $ mp M.! s, rs)
                  )
                (s0, cycle xs)
                | s0 <- filter ((== 'A') . last) $ M.keys mp
              ]
        in  foldr lcm 1 cycleLengths
        -- subtract 1 . length $ iterateMaybe (\(ss,r:rs) ->
        --   guard (not $ all ((== 'Z') . last) ss) $>
        --       (ss <&> \s ->
        --           if r
        --             then (snd $ mp M.! s)
        --             else (fst $ mp M.! s)
        --       , rs)
        --     )
        --   (filter ((== 'A') . last) (M.keys mp), cycle xs)
    }
