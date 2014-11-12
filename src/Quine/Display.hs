{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiWayIf #-}
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
  , handleDisplayEvent
  ) where

import Control.Exception
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class hiding (get)
import Control.Lens
import Data.Bits
import Foreign.C
import Graphics.UI.SDL
import Quine.Env
import Quine.Exception
import Quine.Options
import Quine.SDL
import Quine.StateVar
import System.IO
import Graphics.GL.Core41

-- | Basic OpenGL + SDL display that provides the screen and various bits of metadata about the act of displaying data
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

-- | Complain loudly enough to pop up a window
warn :: (MonadIO m, MonadState s m, HasDisplay s) => String -> String -> m ()
warn t m = do
  window <- use displayWindow
  liftIO $ do 
    hPutStrLn stderr $ "Warning: " ++ t ++ ": " ++ m
    withCString t $ \title -> withCString m $ \message ->
      showSimpleMessageBox SDL_MESSAGEBOX_WARNING title message window >>= err

rescale :: Float -> (Int, Int) -> (Int, Int)
rescale r (w, h) = (floor $ r * fromIntegral w, floor $ r * fromIntegral h)

-- | Recalculate the actual OpenGL viewport size considering Retina
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

-- | Discharge events we should always handle correctly, e.g. CUA concerns for quitting, going full-screen, etc.
handleDisplayEvent :: (MonadIO m, MonadState s m, HasDisplay s, MonadReader e m, HasOptions e) => Event -> m ()
handleDisplayEvent QuitEvent{} = throw Shutdown
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_ENTER } = do
  displayHasMouseFocus .= True
  relativeMouseMode    $= True
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_LEAVE } = do
  displayHasMouseFocus .= False
  relativeMouseMode    $= False
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_FOCUS_GAINED } = do
  displayHasKeyboardFocus .= True
  relativeMouseMode       $= False
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_FOCUS_LOST } = do
  displayHasKeyboardFocus .= False
  relativeMouseMode       $= False
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_MINIMIZED } = do
  displayHasKeyboardFocus .= False
  displayVisible          .= False
  relativeMouseMode       $= False
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_MAXIMIZED } = do
  displayHasKeyboardFocus .= True
  displayVisible          .= True
  relativeMouseMode       $= True
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_HIDDEN } = do
  displayVisible          .= False
  displayHasKeyboardFocus .= False
  relativeMouseMode       $= False
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_EXPOSED } = do
  displayVisible .= True
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_RESTORED } = do
  displayVisible          .= True -- unminimized
  displayHasKeyboardFocus .= True
  relativeMouseMode       $= True
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_SHOWN } = do
  displayHasKeyboardFocus .= True
  displayVisible          .= True
  relativeMouseMode       $= True
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_CLOSE } = liftIO $ throw Shutdown
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_MOVED } = return () -- who cares?
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_NONE } = return () -- who cares?
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_SIZE_CHANGED, windowEventData1 = w, windowEventData2 = h } = do
  displayWindowSize        .= (fromIntegral w, fromIntegral h)
  displayWindowSizeChanged .= True
handleDisplayEvent WindowEvent { eventType = SDL_WINDOWEVENT_RESIZED, windowEventData1 = w, windowEventData2 = h } = do
  displayWindowSize        .= (fromIntegral w, fromIntegral h)
  displayWindowSizeChanged .= True
handleDisplayEvent KeyboardEvent{eventType = SDL_KEYDOWN, keyboardEventKeysym=Keysym{keysymKeycode = SDLK_ESCAPE }} =
  relativeMouseMode $= False -- let escape give us back our mouse pointer
handleDisplayEvent MouseButtonEvent{} =
  relativeMouseMode $= True -- and clicking anywhere can take it over, too
handleDisplayEvent KeyboardEvent{eventType = SDL_KEYDOWN, keyboardEventKeysym=Keysym{keysymKeycode = k, keysymMod = m }}
  | m .&. KMOD_GUI /= 0, k == SDLK_q = throw Shutdown -- CUA Cmd-Q, use keycode "Q" so it can move when they remap
  | m .&. KMOD_GUI /= 0, k == SDLK_RETURN = do        -- CUA Cmd-Return
    fs <- displayFullScreen <%= not
    fsn <- view optionsFullScreenNormal
    w  <- use displayWindow
    _ <- setWindowFullscreen w $ if 
      | not fs    -> 0
      | fsn       -> SDL_WINDOW_FULLSCREEN
      | otherwise -> SDL_WINDOW_FULLSCREEN_DESKTOP
    relativeMouseMode $= True -- steal mouse pointer
    return ()
  | otherwise = relativeMouseMode $= True -- everything else steals back mouse pointer too
handleDisplayEvent _ = return () -- liftIO $ hPrint stderr e
