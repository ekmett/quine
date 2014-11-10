module Quine.Clock
  ( now
  , Time
  , DeltaTime
  ) where

import Control.Monad.IO.Class
import System.Clock

type Time      = Double
type DeltaTime = Double

-- | A simple monotonic system clock in seconds with undefined origin
now :: MonadIO m => m Time
now = liftIO $ do
  TimeSpec s ns <- getTime Monotonic
  return $ fromIntegral s + fromIntegral ns * 1.0e-9
