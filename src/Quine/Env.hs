{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.Env
  ( Env(..)
  , HasEnv(..)
  , frameCounter
  , heightGauge
  , widthGauge
  ) where

import Control.Lens
import Quine.Monitor
import Quine.Options

-- * Environment

data Env = Env
  { _envMonitor   :: Monitor
  , _envOptions   :: Options
  , _frameCounter :: Counter
  , _widthGauge   :: Gauge
  , _heightGauge  :: Gauge
  }

makeLenses ''Env

instance HasMonitor Env where
  monitor = envMonitor

instance HasOptions Env where
  options = envOptions

class (HasMonitor t, HasOptions t) => HasEnv t where
  env :: Lens' t Env

instance HasEnv Env where
  env = id

