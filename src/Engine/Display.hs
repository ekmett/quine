{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Engine.Display 
  ( Display(..)
  , HasDisplay(..)
  , warn
  ) where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens
import Data.Typeable
import Engine.GL.Cache
import Engine.SDL.Exception
import Foreign.C
import Graphics.UI.SDL
import Graphics.UI.SDL.Enum.Pattern

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

warn :: (MonadIO m, MonadState s m, HasDisplay s) => String -> String -> m ()
warn t m = do
  window <- use displayWindow
  liftIO $ withCString t $ \title -> withCString m $ \message ->
    showSimpleMessageBox MessageBoxFlagWarning title message window >>= err
