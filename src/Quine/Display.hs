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
import Graphics.UI.SDL.Enum.Pattern
import Quine.Env
import Quine.Exception
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

-- discharge events we should always handle correctly, e.g. CUA concerns for quitting, going full-screen, etc.
handleDisplayEvent :: (MonadIO m, MonadState s m, HasDisplay s, MonadReader e m, HasOptions e) => Event -> m ()
handleDisplayEvent QuitEvent{} = throw Shutdown
handleDisplayEvent WindowEvent { eventType = WindowEventEnter       } = do
  displayHasMouseFocus .= True
  relativeMouseMode    $= True
handleDisplayEvent WindowEvent { eventType = WindowEventLeave       } = do
  displayHasMouseFocus .= False
  relativeMouseMode    $= False
handleDisplayEvent WindowEvent { eventType = WindowEventFocusGained } = do
  displayHasKeyboardFocus .= True
  relativeMouseMode       $= False
handleDisplayEvent WindowEvent { eventType = WindowEventFocusLost   } = do
  displayHasKeyboardFocus .= False
  relativeMouseMode       $= False
handleDisplayEvent WindowEvent { eventType = WindowEventMinimized   } = do
  displayHasKeyboardFocus .= False
  displayVisible          .= False
  relativeMouseMode       $= False
handleDisplayEvent WindowEvent { eventType = WindowEventMaximized   } = do
  displayHasKeyboardFocus .= True
  displayVisible          .= True
  relativeMouseMode       $= True
handleDisplayEvent WindowEvent { eventType = WindowEventHidden      } = do
  displayVisible          .= False
  displayHasKeyboardFocus .= False
  relativeMouseMode       $= False
handleDisplayEvent WindowEvent { eventType = WindowEventExposed     } = do
  displayVisible .= True
handleDisplayEvent WindowEvent { eventType = WindowEventRestored    } = do
  displayVisible          .= True -- unminimized
  displayHasKeyboardFocus .= True
  relativeMouseMode       $= True
handleDisplayEvent WindowEvent { eventType = WindowEventShown       } = do
  displayHasKeyboardFocus .= True
  displayVisible          .= True
  relativeMouseMode       $= True
handleDisplayEvent WindowEvent { eventType = WindowEventClose       } = liftIO $ throw Shutdown
handleDisplayEvent WindowEvent { eventType = WindowEventMoved       } = return () -- who cares?
handleDisplayEvent WindowEvent { eventType = WindowEventNone        } = return () -- who cares?
handleDisplayEvent WindowEvent { eventType = WindowEventSizeChanged, windowEventData1 = w, windowEventData2 = h } = do
  displayWindowSize        .= (fromIntegral w, fromIntegral h)
  displayWindowSizeChanged .= True
handleDisplayEvent WindowEvent { eventType = WindowEventResized, windowEventData1 = w, windowEventData2 = h } = do
  displayWindowSize        .= (fromIntegral w, fromIntegral h)
  displayWindowSizeChanged .= True
handleDisplayEvent KeyboardEvent{eventType = EventTypeKeyDown, keyboardEventKeysym=Keysym{keysymKeycode = KeycodeEscape }} =
  relativeMouseMode $= False -- let escape give us back our mouse pointer
handleDisplayEvent MouseButtonEvent{} =
  relativeMouseMode $= True -- and clicking anywhere can take it over, too
handleDisplayEvent KeyboardEvent{eventType = EventTypeKeyDown, keyboardEventKeysym=Keysym{keysymKeycode = k, keysymMod = m }}
  | m .&. (KeymodRGUI .|. KeymodLGUI) /= 0, k == KeycodeQ      = throw Shutdown -- CUA Cmd-Q, use keycode "Q" so it can move when they remap
  | m .&. (KeymodRGUI .|. KeymodLGUI) /= 0, k == KeycodeReturn = do             -- CUA Cmd-Return
    fs <- displayFullScreen <%= not
    fsn <- view optionsFullScreenNormal
    w  <- use displayWindow
    _ <- liftIO $ setWindowFullscreen w $ if 
      | not fs    -> 0
      | fsn       -> WindowFlagFullscreen 
      | otherwise -> WindowFlagFullscreenDesktop
    relativeMouseMode $= True -- steal mouse pointer
    return ()
  | otherwise = relativeMouseMode $= True -- everything else steals back mouse pointer too
handleDisplayEvent _ = return () -- liftIO $ hPrint stderr e
