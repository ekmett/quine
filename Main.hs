{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Graphics.GL.Raw.Types
import Graphics.GL.Raw.Profile.Core41
import Graphics.UI.SDL.Enum.Pattern
import Graphics.UI.SDL.Event as SDL
import Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Video as SDL
import Options.Applicative
import Prelude hiding (init)
import Quine.Display
import Quine.Exception
import Quine.GL
import Quine.GL.Error
import Quine.GL.Program
import Quine.GL.Shader
import Quine.Monitor
import Quine.Options
import Quine.SDL
import Quine.StateVar

#include "locations.h"

-- * Environment
data System = System
  { _systemMonitor   :: Monitor
  , _systemOptions   :: Options
  , _systemShaderEnv :: ShaderEnv
  , _frameCounter    :: Counter
  , _widthGauge      :: Gauge
  , _heightGauge     :: Gauge
  } deriving Typeable

makeLenses ''System

instance HasMonitor System where
  monitor = systemMonitor

instance HasOptions System where
  options = systemOptions

instance HasShaderEnv System where
  shaderEnv = systemShaderEnv

class (HasShaderEnv t, HasMonitor t, HasOptions t) => HasSystem t where
  system :: Lens' t System

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

  label "sdl.version" ekg >>= \ lv -> version >>= assign lv . show
 
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
          .|. (if opts^.optionsFullScreen then (if opts^.optionsFullScreenNormal then WindowFlagFullscreen else WindowFlagFullscreenDesktop) else 0)
  window <- createWindow windowName WindowPosCentered WindowPosCentered (fromIntegral w) (fromIntegral h) flags

  -- start OpenGL
  cxt <- glCreateContext window
  makeCurrent window cxt
  label "gl.vendor" ekg          >>= \ lv -> get vendor >>= assign lv
  label "gl.renderer" ekg        >>= \ lv -> get renderer >>= assign lv
  label "gl.version" ekg         >>= \ lv -> get glVersion >>= assign lv
  label "gl.shading.version" ekg >>= \ lv -> get shadingLanguageVersion >>= assign lv
  -- glEnable gl_FRAMEBUFFER_SRGB
  throwErrors
  se <- buildShaderEnv opts
  fc <- counter "quine.frame" ekg
  vw <- gauge "viewport.width" ekg
  vh <- gauge "viewport.height" ekg
  let sys = System ekg opts se fc vw vh
      dsp = Display 
        { _displayWindow            = window
        , _displayGL                = cxt
        , _displayFullScreen        = opts^.optionsFullScreen
        , _displayWindowSize        = Size (fromIntegral w) (fromIntegral h)
        , _displayWindowSizeChanged = True
        , _displayMinimized         = False
        , _displayHasMouseFocus     = True
        , _displayHasKeyboardFocus  = True
        , _displayVisible           = True
        }
  runReaderT (evalStateT core dsp) sys `finally` do
    glDeleteContext cxt
    destroyWindow window
    quit
    exitSuccess
  
core :: (MonadIO m, MonadState s m, HasDisplay s, MonadReader e m, HasSystem e, HasOptions e) => m a
core = do
  screenShader <- compile VertexShader "screen.vert"
  whiteShader <- compile FragmentShader =<< view optionsFragment
  scn <- link screenShader whiteShader
  emptyVAO <- generate
  iResolution <- uni  scn "iResolution"
  iGlobalTime <- unif scn "iGlobalTime"
  epoch <- liftIO getCurrentTime
  throwErrors
  currentProgram $= Just scn
  bindVertexArrayObject $= Just emptyVAO
  forever $ do 
    poll 
    resize 
    render $ do
      liftIO getCurrentTime >>= \now -> iGlobalTime $= realToFrac (diffUTCTime now epoch)
      use displayWindowSize >>= \sz -> iResolution $= toVertex2 sz
      liftIO $ drawArrays Triangles 0 3

-- | 
-- Build a StateVar for getting/setting a uniform
uni :: (MonadIO m, Uniform a) => Program -> String -> m (StateVar a)
uni p u = uniform `liftM` get (uniformLocation p u)

-- | 
-- Build a StateVar for getting/setting a uniform float
--
-- Workaround for <https://github.com/haskell-opengl/OpenGL/issues/64>
unif :: MonadIO m => Program -> String -> m (StateVar GLfloat)
unif p u = (xmap Index1 (\(Index1 a) -> a) . uniform) `liftM` get (uniformLocation p u)

toVertex2 :: Size -> Vertex2 GLfloat
toVertex2 (Size w h) = Vertex2 (fromIntegral w) (fromIntegral h)

rescale :: Float -> (Int, Int) -> (Int, Int)
rescale r (Size w h) = Size (floor $ r * fromIntegral w) (floor $ r * fromIntegral h)

resize :: (MonadIO m, MonadReader e m, HasSystem e, MonadState s m, HasDisplay s) => m ()
resize = do
  win  <- use displayWindow
  opts <- view options
  sz@(Size w h) <- rescale (pointScale opts) `liftM` get (windowSize win) -- retina
  sys <- view system
  assign (sys^.widthGauge)  $ fromIntegral w
  assign (sys^.heightGauge) $ fromIntegral h
  viewport $= (Position 0 0, sz)
  displayWindowSize .= sz

render :: (MonadIO m, MonadReader e m, HasSystem e, MonadState s m, HasDisplay s) => m () -> m ()
render kernel = do
  inc =<< view (system.frameCounter)
  liftIO $ do
    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer, StencilBuffer, DepthBuffer]
  kernel
  w <- use displayWindow
  liftIO $ do
    glFlush
    glSwapWindow w

-- * Polling

poll :: (MonadIO m, MonadState s m, HasDisplay s, MonadReader e m, HasOptions e) => m ()
poll = do
  me <- liftIO $ alloca $ \ep -> do
    r <- pollEvent ep
    if r /= 0 then Just <$> peek ep
              else return Nothing
  case me of
    Just e  -> event e >> poll
    Nothing -> return ()

event :: (MonadIO m, MonadState s m, HasDisplay s, MonadReader e m, HasOptions e) => SDL.Event -> m ()
event QuitEvent{} = throw Shutdown
event WindowEvent { eventType = WindowEventEnter       } = displayHasMouseFocus .= True
event WindowEvent { eventType = WindowEventLeave       } = displayHasMouseFocus .= False
event WindowEvent { eventType = WindowEventFocusGained } = displayHasKeyboardFocus .= True
event WindowEvent { eventType = WindowEventFocusLost   } = displayHasKeyboardFocus .= False
event WindowEvent { eventType = WindowEventMinimized   } = displayVisible .= False
event WindowEvent { eventType = WindowEventMaximized   } = displayVisible .= True
event WindowEvent { eventType = WindowEventHidden      } = displayVisible .= False
event WindowEvent { eventType = WindowEventExposed     } = displayVisible .= True
event WindowEvent { eventType = WindowEventRestored    } = displayVisible .= True -- unminimized
event WindowEvent { eventType = WindowEventShown       } = displayVisible .= True
event WindowEvent { eventType = WindowEventClose       } = liftIO $ throw Shutdown
event WindowEvent { eventType = WindowEventMoved       } = return () -- who cares?
event WindowEvent { eventType = WindowEventNone        } = return () -- who cares?
event WindowEvent { eventType = WindowEventSizeChanged, windowEventData1 = w, windowEventData2 = h } = do
  displayWindowSize        .= Size (fromIntegral w) (fromIntegral h)
  displayWindowSizeChanged .= True
event WindowEvent { eventType = WindowEventResized, windowEventData1 = w, windowEventData2 = h } = do
  displayWindowSize        .= Size (fromIntegral w) (fromIntegral h)
  displayWindowSizeChanged .= True
event KeyboardEvent{eventType = EventTypeKeyDown, keyboardEventKeysym=Keysym{keysymKeycode = k, keysymMod = m }}
  | m .&. (KeymodRGUI .|. KeymodLGUI) /= 0, k == KeycodeQ      = throw Shutdown -- CUA Cmd-Q
  | m .&. (KeymodRGUI .|. KeymodLGUI) /= 0, k == KeycodeReturn = do             -- CUA Cmd-Return
    fs <- displayFullScreen <%= not
    fsn <- view optionsFullScreenNormal
    w  <- use displayWindow
    _ <- liftIO $ setWindowFullscreen w $ if fs then (if fsn then WindowFlagFullscreen else WindowFlagFullscreenDesktop) else 0
    return ()
event _ = return () -- liftIO $ hPrint stderr e

