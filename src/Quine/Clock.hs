module Quine.Clock
  ( now
  ) where

import Control.Monad.IO.Class
import Data.Int
import System.Clock

-- | A simple monotonic system clock counting in nanoseconds with an undefined origin
now :: MonadIO m => m Int64
now = liftIO $ do
  TimeSpec s ns <- getTime Monotonic
  return $ fromIntegral s * 1000000000 + fromIntegral ns
