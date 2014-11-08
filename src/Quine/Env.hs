{-# LANGUAGE TemplateHaskell #-}
module Quine.Env
  ( Env(..)
  , HasEnv(..)
  , frameCounter
  , heightGauge
  , widthGauge
  ) where

import Control.Lens
import Quine.GL
import Quine.Monitor
import Quine.Options

-- * Environment

data Env = Env
  { _envMonitor   :: Monitor
  , _envOptions   :: Options
  , _envShaderEnv :: ShaderEnv
  , _frameCounter :: Counter
  , _widthGauge   :: Gauge
  , _heightGauge  :: Gauge
  }

makeLenses ''Env

instance HasMonitor Env where
  monitor = envMonitor

instance HasOptions Env where
  options = envOptions

instance HasShaderEnv Env where
  shaderEnv = envShaderEnv

class (HasShaderEnv t, HasMonitor t, HasOptions t) => HasEnv t where
  env :: Lens' t Env

instance HasEnv Env where
  env = id

