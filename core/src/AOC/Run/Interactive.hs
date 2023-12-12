-- |
-- Module      : AOC.Run.Interactive
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Versions of loaders and runners meant to be used in GHCI.
module AOC.Run.Interactive
  ( -- * Fetch and Run

    -- ** Return Answers
    execSolution,
    execSolutionWith,
    testSolution,
    viewPrompt,
    waitForPrompt,
    submitSolution,

    -- ** No Answers
    execSolution_,
    execSolutionWith_,
    testSolution_,
    viewPrompt_,
    waitForPrompt_,
    submitSolution_,

    -- * Load Inputs
    loadInput,
    loadParseInput,
    loadTests,
    loadParseTests,

    -- * Util
    mkSpec,
  )
where

import AOC.Discover
import AOC.Run
import AOC.Run.Config
import AOC.Run.Load
import AOC.Solver
import AOC.Util
import Advent
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Bifunctor
import qualified Data.Map as M
import Data.Text (Text)

-- | Run the solution indicated by the challenge spec on the official
-- puzzle input.  Get answer as result.
execSolution :: ChallengeBundle -> ChallengeSpec -> IO String
execSolution cb cs = eitherIO $ do
  cfg <- liftIO $ configFile defConfPath
  out <- mainRun cb cfg . defaultMRO $ TSPart cs
  res <-
    maybeToEither ["Result not found in result map (Internal Error)"] $
      M.lookup cs out
  liftEither $ snd res

-- | Run the solution indicated by the challenge spec on a custom input.
-- Get answer as result.
execSolutionWith ::
  ChallengeBundle ->
  ChallengeSpec ->
  -- | custom puzzle input
  String ->
  IO String
execSolutionWith cb cs inp = eitherIO $ do
  cfg <- liftIO $ configFile defConfPath
  out <-
    mainRun cb cfg $
      (defaultMRO (TSPart cs))
        { _mroInput = \_ -> pure $ Just inp
        }
  res <-
    maybeToEither ["Result not found in result map (Internal Error)"] $
      M.lookup cs out
  liftEither $ snd res

-- | Run test suite for a given challenge spec.
--
-- Returns 'Just' if any tests were run, with a 'Bool' specifying whether
-- or not all tests passed.
testSolution :: ChallengeBundle -> ChallengeSpec -> IO (Maybe Bool)
testSolution cb cs = eitherIO $ do
  cfg <- liftIO $ configFile defConfPath
  out <-
    mainRun cb cfg $
      (defaultMRO (TSPart cs))
        { _mroTest = True
        }
  res <-
    maybeToEither ["Result not found in result map (Internal Error)"] $
      M.lookup cs out
  pure $ fst res

-- | View the prompt for a given challenge spec.
viewPrompt :: ChallengeBundle -> ChallengeSpec -> IO Text
viewPrompt cb cs = eitherIO $ do
  cfg <- liftIO $ configFile defConfPath
  out <- mainView cb cfg . defaultMVO $ TSPart cs
  maybeToEither ["Prompt not found in result map (Internal Error)"] $
    M.lookup cs out

-- | Countdown to get the prompt for a given challenge spec, if not yet
-- available.
waitForPrompt :: ChallengeBundle -> ChallengeSpec -> IO Text
waitForPrompt cb cs = eitherIO $ do
  cfg <- liftIO $ configFile defConfPath
  out <-
    mainView cb cfg $
      (defaultMVO (TSPart cs))
        { _mvoWait = True
        }
  maybeToEither ["Prompt not found in result map (Internal Error)"] $
    M.lookup cs out

-- | Submit solution for a given challenge spec, and lock if correct.
submitSolution :: ChallengeBundle -> ChallengeSpec -> IO (Text, SubmitRes)
submitSolution cb cs = eitherIO $ do
  cfg <- liftIO $ configFile defConfPath
  mainSubmit cb cfg cs defaultMSO

-- | Result-suppressing version of 'execSolution'.
execSolution_ :: ChallengeBundle -> ChallengeSpec -> IO ()
execSolution_ cb = void . execSolution cb

-- | Result-suppressing version of 'execSolutionWith'.
execSolutionWith_ ::
  ChallengeBundle ->
  ChallengeSpec ->
  -- | custom puzzle input
  String ->
  IO ()
execSolutionWith_ cb cs = void . execSolutionWith cb cs

-- | Result-suppressing version of 'testSolution'.
testSolution_ :: ChallengeBundle -> ChallengeSpec -> IO ()
testSolution_ cb = void . testSolution cb

-- | Result-suppressing version of 'viewPrompt'.
viewPrompt_ :: ChallengeBundle -> ChallengeSpec -> IO ()
viewPrompt_ cb = void . viewPrompt cb

-- | Result-suppressing version of 'waitForPrompt'.
waitForPrompt_ :: ChallengeBundle -> ChallengeSpec -> IO ()
waitForPrompt_ cb = void . waitForPrompt cb

-- | Result-suppressing version of 'submitSolution'.
submitSolution_ :: ChallengeBundle -> ChallengeSpec -> IO ()
submitSolution_ cb = void . submitSolution cb

-- | Run the parser of a solution, given its 'ChallengeSpec'.
--
-- @
-- 'loadParseInput' (solSpec 'day01a) day01a
-- @
loadParseInput :: Integer -> ChallengeSpec -> a :~> b -> IO a
loadParseInput yr cs s = eitherIO $ do
  i <- liftIO $ loadInput yr cs
  maybeToEither ["No parse"] $ sParse s i

-- | Run the parser of a solution on test data, given its 'ChallengeSpec'.
--
-- @
-- 'loadParseTests' (solSpec 'day01a) day01a
-- @
loadParseTests :: Integer -> ChallengeSpec -> a :~> b -> IO [(Maybe a, TestMeta)]
loadParseTests yr cs s = (map . first) (sParse s) <$> loadTests yr cs

-- | Load input for a given challenge
loadInput :: Integer -> ChallengeSpec -> IO String
loadInput yr cs = eitherIO $ do
  CD {..} <- liftIO $ do
    Cfg {..} <- configFile defConfPath
    challengeData _cfgSession yr cs
  liftEither _cdInput

-- | Load test cases for a given challenge
loadTests :: Integer -> ChallengeSpec -> IO [(String, TestMeta)]
loadTests yr cs = do
  Cfg {..} <- configFile defConfPath
  _cdTests <$> challengeData _cfgSession yr cs

-- | Unsafely create a 'ChallengeSpec' from a day number and part.
--
-- Is undefined if given a day number out of range (1-25).
mkSpec :: Integer -> Part -> ChallengeSpec
mkSpec = CS . mkDay_

eitherIO :: ExceptT [String] IO a -> IO a
eitherIO act =
  runExceptT act >>= \case
    Right x -> pure x
    Left es -> fail $ unlines es
