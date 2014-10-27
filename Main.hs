{-# LANGUAGE TemplateHaskell, PatternSynonyms, DeriveDataTypeable, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
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
 -- , _worldHdr     :: !FramebufferObject
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

    -- fbo <- generate
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

    let cleanup = do
         glDeleteContext window
         destroyWindow window
         quit
         exitSuccess

    finally cleanup $ runReaderT ?? System mon opts se $ do
      poll <- buildPoll
      screenShader <- compile VertexShader   "screen.vert"
      whiteShader  <- compile FragmentShader "white.frag"
      prog <- link screenShader whiteShader
      evalStateT (forever $ poll >> render) $ World prog disp

    -- cleanup
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

-- * Polling

data Shutdown = Shutdown deriving (Show,Typeable)
instance Exception Shutdown

buildPoll :: (MonadIO n, MonadIO m, MonadState s m, HasDisplay s, MonadReader e m, HasOptions e) => n (m ())
buildPoll = liftIO $ do
  ep <- malloc
  let poll = do
        r <- liftIO (pollEvent ep)
        when (r /= 0) $ do
          e <- liftIO (peek ep)
          event e
          poll
  return poll

guiKey :: (MonadIO m, MonadState s m, HasDisplay s, MonadReader e m, HasOptions e) => Keycode -> m ()
guiKey KeycodeQ = throw Shutdown
guiKey KeycodeReturn = do
  fs <- displayFullScreen <%= not
  fsd <- view optionsFullScreenDesktop
  w  <- use displayWindow
  _ <- liftIO $ setWindowFullscreen w $ if fs then (if fsd then WindowFlagFullscreenDesktop else WindowFlagFullscreen) else 0
  return ()
guiKey e = liftIO $ hPrint stderr $ "Command " ++ show e

event :: (MonadIO m, MonadState s m, HasDisplay s, MonadReader e m, HasOptions e) => SDL.Event -> m ()
event QuitEvent{} = throw Shutdown
event KeyboardEvent{eventType = EventTypeKeyDown, keyboardEventKeysym=Keysym{keysymKeycode = k, keysymMod = m }}
  | m .&. (KeymodRGUI .|. KeymodLGUI) /= 0 = guiKey k
event WindowEvent { eventType = WindowEventResized     } = return () -- eventd during size change, since we only get that in the event of an OS change
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
event e = liftIO $ hPrint stderr e -- uneventd event

-- WindowEvent {eventType = 512, eventTimestamp = 231, windowEventWindowID = 1, windowEventEvent = 12, windowEventData1 = 0, windowEventData2 = 0}
