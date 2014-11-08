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
module Quine.Display 
  ( Display(..)
  , HasDisplay(..)
  , warn
  ) where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens
import Foreign.C
import Graphics.UI.SDL
import Graphics.UI.SDL.Enum.Pattern
import Quine.SDL
import System.IO

-- basic opengl + sdl display for the screen, etc.

data Display = Display 
  { _displayWindow            :: !Window
  , _displayGL                :: !GLContext
  , _displayFullScreen        :: !Bool
  , _displayWindowSize        :: (Int, Int)
  , _displayWindowSizeChanged :: !Bool
  , _displayMinimized         :: !Bool
  , _displayHasMouseFocus     :: !Bool
  , _displayHasKeyboardFocus  :: !Bool
  , _displayVisible           :: !Bool
  }

makeClassy ''Display

-- | complain loudly enough to pop up a window
warn :: (MonadIO m, MonadState s m, HasDisplay s) => String -> String -> m ()
warn t m = do
  window <- use displayWindow
  liftIO $ do 
    hPutStrLn stderr $ "Warning: " ++ t ++ ": " ++ m
    withCString t $ \title -> withCString m $ \message ->
      showSimpleMessageBox MessageBoxFlagWarning title message window >>= err
