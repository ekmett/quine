{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Engine.Display where

import Control.Lens
import Data.Typeable
import Engine.GL.Cache
import Graphics.UI.SDL

-- basic opengl + sdl display for the screen, etc.

data Display = Display 
  { _displayWindow     :: !Window
  , _displayGL         :: !GLContext
  , _displayFullScreen :: !Bool
  , _displayCaches     :: !Caches
  } deriving Typeable

makeClassy ''Display

instance HasCaches Display where
  caches = displayCaches
