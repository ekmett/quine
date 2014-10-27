{-# LANGUAGE TemplateHaskell, PatternSynonyms, DeriveDataTypeable, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Lens hiding (assign)
import Control.Monad hiding (forM_)
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Data.Monoid
import Data.Text.Lens
import Data.Typeable
import Engine.Display
import Engine.Monitor
import Engine.Options
import Engine.GL.Shader
import Engine.SDL.Basic
import Engine.SDL.Video
import Foreign
import Foreign.C
import System.Exit
import System.IO
import Graphics.Rendering.OpenGL as GL hiding (doubleBuffer)
import Graphics.Rendering.OpenGL.Raw as GL
import Graphics.UI.SDL.Enum.Pattern as SDL
import Graphics.UI.SDL.Event as SDL
import Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Video as SDL
import Options.Applicative
import Prelude hiding (init)

-- * Environment
data System = System
  { _systemMonitor   :: Monitor
  , _systemOptions   :: Options
  , _systemShaderEnv :: ShaderEnv
  } deriving Typeable

makeClassy ''System

instance HasMonitor System where
  monitor = systemMonitor

instance HasOptions System where
  options = systemOptions

instance HasShaderEnv System where
  shaderEnv = systemShaderEnv

-- * State

data World = World
  { _worldProgram :: !Program
  , _worldDisplay :: !Display
  } deriving Typeable

makeClassy ''World

instance HasDisplay World where
  display = worldDisplay

-- * Setup

main :: IO ()
main = runInBoundThread $ withCString "engine" $ \windowName -> do
  optsParser <- parseOptions

  opts <- execParser $ info (helper <*> optsParser) $
    fullDesc
    <> progDesc "engine"
    <> header "Engine"

  withMonitor opts $ \mon -> do -- start up monitoring
    label "sdl.version" mon >>= \ lv -> version >>= \v -> assign lv $ show v ^. packed
 
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
            .|. (if opts^.optionsFullScreen then WindowFlagFullscreen else 0)
    window <- createWindow windowName WindowPosUndefined WindowPosUndefined (fromIntegral w) (fromIntegral h) flags
    cxt <- glCreateContext window
    glEnable gl_FRAMEBUFFER_SRGB
    se <- buildShaderEnv opts
    let disp = Display 
          { _displayWindow = window
          , _displayGL     = cxt
          , _displayCaches = def
          , _displayFullScreen = opts^.optionsFullScreen
          , _displayWindowSize = Size (fromIntegral w) (fromIntegral h)
          , _displayWindowSizeChanged = True
          , _displayMinimized = False
          , _displayHasMouseFocus = True
          , _displayHasKeyboardFocus = True
          , _displayVisible = True
          }
    runReaderT ?? System mon opts se $ do
      poll <- buildPoll
      screenShader <- compile VertexShader   "screen.vert"
      whiteShader  <- compile FragmentShader "white.frag"
      prog <- link screenShader whiteShader
      evalStateT ?? World prog disp $ forever (poll >> render)
  
-- * Rendering

render :: (MonadIO m, MonadState s m, HasWorld s, HasDisplay s) => m ()
render = do
  w <- use displayWindow
  use displayWindowSizeChanged >>= \c -> when c $ do
    sz <- use displayWindowSize
    liftIO $ viewport $= (Position 0 0, sz)
    -- are we going to need to destroy everything, like we used to?
    displayWindowSizeChanged .= False
  liftIO $ do
    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer, StencilBuffer, DepthBuffer]
  use worldProgram >>= \p -> liftIO (currentProgram $= Just p)
  -- run this thing bindlessly
  liftIO $ glSwapWindow w

shutdown :: MonadIO m => m ()
shutdown = liftIO $ quit >> exitSuccess

-- * Polling

buildPoll :: (MonadIO n, MonadIO m, MonadState s m, HasDisplay s) => n (m ())
buildPoll = liftIO $ do
  ep <- malloc
  let poll = do
        r <- liftIO (pollEvent ep)
        when (r /= 0) $ do
          e <- liftIO (peek ep)
          handle e
          poll
  return poll

guiKey :: (MonadIO m, MonadState s m, HasDisplay s) => Keycode -> m ()
guiKey KeycodeQ = shutdown
guiKey KeycodeReturn = do
  fs <- displayFullScreen <%= not
  w  <- use displayWindow
  _ <- liftIO $ setWindowFullscreen w $ if fs then WindowFlagFullscreenDesktop else 0
  return ()
guiKey e = liftIO $ hPrint stderr $ "Command " ++ show e

handle :: (MonadIO m, MonadState s m, HasDisplay s) => SDL.Event -> m ()
handle QuitEvent{} = shutdown
handle KeyboardEvent{eventType = EventTypeKeyDown, keyboardEventKeysym=Keysym{keysymKeycode = k, keysymMod = m }}
  | m .&. (KeymodRGUI .|. KeymodLGUI) /= 0 = guiKey k
handle WindowEvent { eventType = WindowEventResized     } = return () -- handled during size change, since we only get that in the event of an OS change
handle WindowEvent { eventType = WindowEventEnter       } = displayHasMouseFocus .= True
handle WindowEvent { eventType = WindowEventLeave       } = displayHasMouseFocus .= False
handle WindowEvent { eventType = WindowEventFocusGained } = displayHasKeyboardFocus .= True
handle WindowEvent { eventType = WindowEventFocusLost   } = displayHasKeyboardFocus .= False
handle WindowEvent { eventType = WindowEventMinimized   } = displayVisible .= False
handle WindowEvent { eventType = WindowEventMaximized   } = displayVisible .= True
handle WindowEvent { eventType = WindowEventHidden      } = displayVisible .= False
handle WindowEvent { eventType = WindowEventExposed     } = displayVisible .= True
handle WindowEvent { eventType = WindowEventRestored    } = displayVisible .= True -- unminimized
handle WindowEvent { eventType = WindowEventShown       } = displayVisible .= True
handle WindowEvent { eventType = WindowEventClose       } = shutdown
handle WindowEvent { eventType = WindowEventMoved       } = return () -- who cares?
handle WindowEvent { eventType = WindowEventNone        } = return () -- who cares?
handle WindowEvent { eventType = WindowEventSizeChanged, windowEventData1 = w, windowEventData2 = h } = do
  displayWindowSize        .= Size (fromIntegral w) (fromIntegral h)
  displayWindowSizeChanged .= True
handle e = liftIO $ hPrint stderr e -- unhandled event

-- WindowEvent {eventType = 512, eventTimestamp = 231, windowEventWindowID = 1, windowEventEvent = 12, windowEventData1 = 0, windowEventData2 = 0}
