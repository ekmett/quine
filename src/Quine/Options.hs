{-# LANGUAGE CPP #-}
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
module Quine.Options
  ( Options(..)
  , HasOptions(..)
  , parseOptions
  , pointScale
  ) where

import Control.Applicative
import Control.Lens
import Data.Default
import Options.Applicative
#ifdef RELEASE
import Paths_quine
#endif
import Prelude hiding (init)
import Quine.Monitor

data Options = Options
  { _optionsMonitorOptions   :: MonitorOptions
  , _optionsFullScreen       :: !Bool
  , _optionsFullScreenNormal :: !Bool
  , _optionsHighDPI          :: !Bool
  , _optionsHighDPIRatio     :: !Float
  , _optionsWindowWidth      :: !Int
  , _optionsWindowHeight     :: !Int
  , _optionsFragment         :: !FilePath
  , _optionsDebug            :: !Bool
  }

makeClassy ''Options

instance HasMonitorOptions Options where
  monitorOptions = optionsMonitorOptions

-- we need to set up the data directory first
parseOptions :: Parser Options
parseOptions = Options 
       <$> parseMonitorOptions
       <*> switch (long "full-screen" <> short 'f' <> help "open full-screen on launch")
       <*> switch (long "real-full-screen" <> short 'n' <> help "use real full screen; exiting is buggy on OS X with SDL 2.0.3")
       <*> switch (long "retina" <> short 'r' <> help "exploit a retina display if it available")
       <*> option auto (long "scale"  <> short 's' <> help "retina pixel ratio" <> metavar "RATIO" <> value 2.0)
       <*> option auto (long "width" <> short 'x' <> help "window width in pixels" <> metavar "WIDTH" <> value 800)
       <*> option auto (long "height" <> short 'y' <> help "window height in pixels" <> metavar "HEIGHT" <> value 600)
       <*> strOption (long "fragment" <> short 'F' <> help "fragment shader" <> metavar "FILE" <> action "file" <> value "shaders/cartoon.frag")
       <*> switch (long "debug" <> help "turn on synchronous opengl debugging if available")

instance Default Options where
  def = Options def False False False 2.0 1024 768 "shaders/generators.frag" False

pointScale :: Options -> Float
pointScale opts = if opts^.optionsHighDPI then opts^.optionsHighDPIRatio else 1
