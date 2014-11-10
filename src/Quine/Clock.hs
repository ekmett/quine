module Quine.Clock
  ( now
  , Time
  , DeltaTime
  ) where

import Control.Monad.IO.Class
import Data.Int
import System.Clock

type Time      = Int64
type DeltaTime = Int64

-- | A simple monotonic system clock counting in nanoseconds with an undefined origin
now :: MonadIO m => m Time
now = liftIO $ do
  TimeSpec s ns <- getTime Monotonic
  return $ fromIntegral s * 1000000000 + fromIntegral ns
