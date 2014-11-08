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
  , resizeDisplay
  ) where

import Control.Monad (liftM)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class hiding (get)
import Control.Lens
import Foreign.C
import Graphics.UI.SDL
import Graphics.UI.SDL.Enum.Pattern
import Quine.Env
import Quine.Options
import Quine.SDL
import Quine.StateVar
import System.IO
import Graphics.GL.Core41

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

rescale :: Float -> (Int, Int) -> (Int, Int)
rescale r (w, h) = (floor $ r * fromIntegral w, floor $ r * fromIntegral h)

resizeDisplay :: (MonadIO m, MonadReader e m, HasEnv e, MonadState s m, HasDisplay s) => m ()
resizeDisplay = do
  win  <- use displayWindow
  opts <- view options
  sz@(w,h) <- rescale (pointScale opts) `liftM` get (windowSize win)
  sys <- view env
  sys^.widthGauge  $= fromIntegral w
  sys^.heightGauge $= fromIntegral h
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  displayWindowSize .= sz
