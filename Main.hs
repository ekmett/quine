{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Typeable
import Foreign
import Foreign.C
import GHC.Conc
import System.Exit
import System.IO
import Graphics.GL.Core41
import Graphics.UI.SDL.Enum.Pattern
import Graphics.UI.SDL.Event as SDL
import Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Video as SDL
import Options.Applicative
import Prelude hiding (init)
import Quine.Debug
import Quine.Display
import Quine.Exception
import Quine.GL
import Quine.GL.Error
import Quine.GL.Object
import Quine.GL.Program
import Quine.GL.Uniform
import Quine.GL.Version as GL
import Quine.GL.VertexArray
import Quine.Input
import Quine.Monitor
import Quine.Options
import Quine.SDL as SDL
import Quine.StateVar

#include "locations.h"

-- * Environment
data Env = Env
  { _envMonitor   :: Monitor
  , _envOptions   :: Options
  , _envShaderEnv :: ShaderEnv
  , _frameCounter    :: Counter
  , _widthGauge      :: Gauge
  , _heightGauge     :: Gauge
  } deriving Typeable

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

data System = System
  { _systemDisplay :: Display
  , _systemInput   :: Input
  , _focused       :: Bool
  } deriving Typeable

makeLenses ''System

class (HasDisplay t, HasInput t) => HasSystem t where
  system :: Lens' t System

instance HasDisplay System where
  display = systemDisplay

instance HasInput System where
  input = systemInput

instance HasSystem System where
  system = id


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
  runReaderT (evalStateT core $ System dsp def True) sys `finally` do
    glDeleteContext cxt
    destroyWindow window
    quit
    exitSuccess
  
core :: (MonadIO m, MonadState s m, HasSystem s, MonadReader e m, HasEnv e, HasOptions e) => m a
core = do
  screenShader <- compile GL_VERTEX_SHADER "screen.vert"
  whiteShader <- compile GL_FRAGMENT_SHADER =<< view optionsFragment
  scn <- link screenShader whiteShader
  emptyVAO <- gen
  iResolution <- uniformLocation scn "iResolution"
  iGlobalTime <- uniformLocation scn "iGlobalTime"
  epoch <- liftIO getCurrentTime
  throwErrors
  currentProgram   $= scn
  boundVertexArray $= emptyVAO
  forever $ do 
    poll 
    resize 
    render $ do
      liftIO getCurrentTime >>= \now   -> glUniform1f iGlobalTime $ realToFrac $ diffUTCTime now epoch
      use displayWindowSize >>= \(w,h) -> glUniform2f iResolution (fromIntegral w) (fromIntegral h)
      glDrawArrays GL_TRIANGLES 0 3

rescale :: Float -> (Int, Int) -> (Int, Int)
rescale r (w, h) = (floor $ r * fromIntegral w, floor $ r * fromIntegral h)

resize :: (MonadIO m, MonadReader e m, HasEnv e, MonadState s m, HasDisplay s) => m ()
resize = do
  win  <- use displayWindow
  opts <- view options
  sz@(w,h) <- rescale (pointScale opts) `liftM` get (windowSize win) -- retina
  sys <- view env
  (sys^.widthGauge)  $= fromIntegral w
  (sys^.heightGauge) $= fromIntegral h
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  displayWindowSize .= sz

render :: (MonadIO m, MonadReader e m, HasEnv e, MonadState s m, HasDisplay s) => m () -> m ()
render kernel = do
  inc =<< view (env.frameCounter)
  glClearColor 0 0 0 1
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  kernel
  w <- use displayWindow
  glFlush
  liftIO $ glSwapWindow w

-- * Polling

poll :: (MonadIO m, MonadState s m, HasSystem s, MonadReader e m, HasOptions e) => m ()
poll = do
  me <- liftIO $ alloca $ \ep -> do
    r <- pollEvent ep
    if r /= 0 then Just <$> peek ep
              else return Nothing
  case me of
    Just e  -> do
      handleWindowEvent e
      handleInputEvent e
      poll
    Nothing -> return ()

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
