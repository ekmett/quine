module Quine.Clock
  ( now
  , Time
  , DeltaTime
  ) where

import Control.Monad.IO.Class
import Data.Functor
import Graphics.UI.SDL
import System.IO.Unsafe

type Time      = Double
type DeltaTime = Double

granularity :: Double
granularity = unsafePerformIO (recip . fromIntegral <$> getPerformanceFrequency)
{-# NOINLINE granularity #-}

-- | A simple monotonic system clock in seconds with undefined origin
now :: MonadIO m => m Time
now = liftIO $ (granularity *) . fromIntegral <$> getPerformanceCounter
