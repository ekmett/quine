{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens hiding (assign)
import Control.Lens.Extras (is)
import Control.Monad hiding (forM_)
import Control.Monad.Reader
import Control.Monad.State hiding (get)
import Data.Default
import Data.Monoid
import Data.Time.Clock
import Foreign
import Foreign.C
import GHC.Conc
import System.Exit
import System.IO
import Graphics.GL.Core41
import Graphics.UI.SDL.Enum.Pattern
import Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Video as SDL
import Linear
import Options.Applicative
import Prelude hiding (init)
import Quine.Camera
import Quine.Env
import Quine.Debug
import Quine.Display
import Quine.Exception
import Quine.GL
import Quine.GL.Error
import Quine.GL.Object
import Quine.GL.Program
import Quine.GL.Types
import Quine.GL.Uniform
import Quine.GL.Version as GL
import Quine.GL.VertexArray
import Quine.Input
import Quine.Monitor
import Quine.Options
import Quine.SDL as SDL
import Quine.StateVar
import Quine.System

#include "locations.h"

-- * State

main :: IO ()
-- main is always bound, but what about from ghci?
main = runInBoundThread $ withCString "quine" $ \windowName -> do
  -- parse options
  optsParser <- parseOptions
  opts <- execParser $ info (helper <*> optsParser) $
    fullDesc
    <> progDesc "quine"
    <> header "Quine"

  -- be careful with exceptions
  setUncaughtExceptionHandler $ \ e -> if
    | is _Shutdown e -> return ()
    | otherwise -> do
      hPrint stderr e
      exitFailure

  -- set up EKG
  ekg <- forkMonitor opts

  label "sdl.version" ekg >>= ($= show SDL.version)
 
  -- start SDL
  init InitFlagEverything
  contextMajorVersion $= 4
  contextMinorVersion $= 1
  contextProfileMask  $= GLProfileCore
  redSize   $= 5
  greenSize $= 5
  blueSize  $= 5
  depthSize $= 16
  doubleBuffer $= True
  let w = opts^.optionsWindowWidth
      h = opts^.optionsWindowHeight
      flags = WindowFlagOpenGL
          .|. WindowFlagShown
          .|. WindowFlagResizable
          .|. (if opts^.optionsHighDPI then WindowFlagAllowHighDPI else 0)
          .|. (if | not (opts^.optionsFullScreen) -> 0
                  | opts^.optionsFullScreenNormal -> WindowFlagFullscreen 
                  | otherwise                     -> WindowFlagFullscreenDesktop)
  window <- createWindow windowName WindowPosCentered WindowPosCentered (fromIntegral w) (fromIntegral h) flags

  -- start OpenGL
  cxt <- glCreateContext window
  makeCurrent window cxt

  when (opts^.optionsDebug) installDebugHook

  label "gl.vendor" ekg           >>= ($= vendor)
  label "gl.renderer" ekg         >>= ($= renderer)
  label "gl.version" ekg          >>= ($= show GL.version)
  label "gl.shading.version" ekg  >>= ($= show shadingLanguageVersion)
  label "gl.shading.versions" ekg >>= ($= show shadingLanguageVersions)
  
  -- glEnable gl_FRAMEBUFFER_SRGB

  throwErrors
  se <- buildShaderEnv opts
  fc <- counter "quine.frame" ekg
  vw <- gauge "viewport.width" ekg
  vh <- gauge "viewport.height" ekg
  let sys = Env ekg opts se fc vw vh
      dsp = Display 
        { _displayWindow            = window
        , _displayGL                = cxt
        , _displayFullScreen        = opts^.optionsFullScreen
        , _displayWindowSize        = (fromIntegral w, fromIntegral h)
        , _displayWindowSizeChanged = True
        , _displayMinimized         = False
        , _displayHasMouseFocus     = True
        , _displayHasKeyboardFocus  = True
        , _displayVisible           = True
        }
  relativeMouseMode $= True -- switch to relative mouse mouse initially
  runReaderT (evalStateT core $ System dsp def def) sys `finally` do
    glDeleteContext cxt
    destroyWindow window
    quit
    exitSuccess

translate :: Vec3 -> Mat4
translate v = eye4 & translation .~ v

core :: (MonadIO m, MonadState s m, HasSystem s, MonadReader e m, HasEnv e, HasOptions e) => m a
core = do
  screenShader <- compile GL_VERTEX_SHADER "screen.vert"
  whiteShader <- compile GL_FRAGMENT_SHADER =<< view optionsFragment
  scn <- link screenShader whiteShader
  emptyVAO <- gen
  iResolution  <- uniformLocation scn "iResolution"
  iGlobalTime  <- uniformLocation scn "iGlobalTime"
  iPerspective <- uniformLocation scn "iPerspective"
  iView        <- uniformLocation scn "iView"
  epoch <- liftIO getCurrentTime
  throwErrors
  currentProgram   $= scn
  boundVertexArray $= emptyVAO
  forever $ do 
    poll $ \e -> handleWindowEvent e >> handleInputEvent e
    resizeDisplay 
    updateCamera
    render $ do
      (w,h) <- use displayWindowSize
      let wf = fromIntegral w
          hf = fromIntegral h
      glUniform2f iResolution wf hf

      c <- use camera
      uniformMat4 iPerspective $ perspective (c^.fov) (wf/hf) 1 65536
      let cameraQuat = axisAngle (V3 1 0 0) (c^.pitch) * axisAngle (V3 0 1 0) (c^.yaw)
      uniformMat4 iView $ m33_to_m44 (fromQuaternion cameraQuat)
                   -- !*! translate (-c^.cameraPos)

      now <- liftIO getCurrentTime
      glUniform1f iGlobalTime $ realToFrac $ diffUTCTime now epoch

      glDrawArrays GL_TRIANGLES 0 3

render :: (MonadIO m, MonadReader e m, HasEnv e, MonadState s m, HasDisplay s) => m () -> m ()
render kernel = do
  inc =<< view (env.frameCounter)
  glClearColor 0 0 0 1
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  kernel
  w <- use displayWindow
  glFlush
  liftIO $ glSwapWindow w

-- discharge events we should always handle correctly, e.g. CUA concerns for quitting, going full-screen, etc.
handleWindowEvent :: (MonadIO m, MonadState s m, HasSystem s, MonadReader e m, HasOptions e) => SDL.Event -> m ()
handleWindowEvent QuitEvent{} = throw Shutdown
handleWindowEvent WindowEvent { eventType = WindowEventEnter       } = do
  displayHasMouseFocus .= True
  relativeMouseMode    $= True
handleWindowEvent WindowEvent { eventType = WindowEventLeave       } = do
  displayHasMouseFocus .= False
  relativeMouseMode    $= False
handleWindowEvent WindowEvent { eventType = WindowEventFocusGained } = do
  displayHasKeyboardFocus .= True
  relativeMouseMode       $= False
handleWindowEvent WindowEvent { eventType = WindowEventFocusLost   } = do
  displayHasKeyboardFocus .= False
  relativeMouseMode       $= False
handleWindowEvent WindowEvent { eventType = WindowEventMinimized   } = do
  displayHasKeyboardFocus .= False
  displayVisible          .= False
  relativeMouseMode       $= False
handleWindowEvent WindowEvent { eventType = WindowEventMaximized   } = do
  displayHasKeyboardFocus .= True
  displayVisible          .= True
  relativeMouseMode       $= True
handleWindowEvent WindowEvent { eventType = WindowEventHidden      } = do
  displayVisible          .= False
  displayHasKeyboardFocus .= False
  relativeMouseMode       $= False
handleWindowEvent WindowEvent { eventType = WindowEventExposed     } = do
  displayVisible .= True
handleWindowEvent WindowEvent { eventType = WindowEventRestored    } = do
  displayVisible          .= True -- unminimized
  displayHasKeyboardFocus .= True
  relativeMouseMode       $= True
handleWindowEvent WindowEvent { eventType = WindowEventShown       } = do
  displayHasKeyboardFocus .= True
  displayVisible          .= True
  relativeMouseMode       $= True
handleWindowEvent WindowEvent { eventType = WindowEventClose       } = liftIO $ throw Shutdown
handleWindowEvent WindowEvent { eventType = WindowEventMoved       } = return () -- who cares?
handleWindowEvent WindowEvent { eventType = WindowEventNone        } = return () -- who cares?
handleWindowEvent WindowEvent { eventType = WindowEventSizeChanged, windowEventData1 = w, windowEventData2 = h } = do
  displayWindowSize        .= (fromIntegral w, fromIntegral h)
  displayWindowSizeChanged .= True
handleWindowEvent WindowEvent { eventType = WindowEventResized, windowEventData1 = w, windowEventData2 = h } = do
  displayWindowSize        .= (fromIntegral w, fromIntegral h)
  displayWindowSizeChanged .= True
handleWindowEvent KeyboardEvent{eventType = EventTypeKeyDown, keyboardEventKeysym=Keysym{keysymKeycode = KeycodeEscape }} =
  relativeMouseMode $= False -- let escape give us back our mouse pointer
handleWindowEvent MouseButtonEvent{} =
  relativeMouseMode $= True -- and clicking anywhere can take it over, too
handleWindowEvent KeyboardEvent{eventType = EventTypeKeyDown, keyboardEventKeysym=Keysym{keysymKeycode = k, keysymMod = m }}
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
handleWindowEvent _ = return () -- liftIO $ hPrint stderr e
