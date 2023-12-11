-- |
-- Module      : AOC.Util
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Assorted utility functions and orphans used for solutions.
module AOC.Util
  ( strip,
    stripNewline,
    eitherToMaybe,
    maybeToEither,
  )
where

import Control.Applicative
import Control.Monad.Except
import qualified Data.Text as T

-- | Strip trailing and leading whitespace.
strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- | Strip trailing newline
stripNewline :: String -> String
stripNewline = reverse . dropWhile (== '\n') . reverse

-- | Convert an 'Either' into a 'Maybe', or any 'Alternative' instance,
-- forgetting the error value.
eitherToMaybe :: (Alternative m) => Either e a -> m a
eitherToMaybe = either (const empty) pure

-- | Convert a 'Maybe' into an 'Either', or any 'MonadError' instance, by
-- providing an error value in case 'Nothing' was given.
maybeToEither :: (MonadError e m) => e -> Maybe a -> m a
maybeToEither e = maybe (throwError e) pure
