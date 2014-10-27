{-# LANGUAGE TemplateHaskell, PatternSynonyms, DeriveDataTypeable, OverloadedStrings #-}
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
import Control.Exception.Lens
import Control.Lens hiding (assign)
import Control.Monad hiding (forM_)
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Data.Monoid
import Data.Text.Lens
import Data.Typeable
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
import Quine.Display
import Quine.GL.Shader
import Quine.Monitor
import Quine.Options
import Quine.Shutdown
import Quine.SDL.Basic
import Quine.SDL.Video

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
main = runInBoundThread $ withCString "quine" $ \windowName -> do
  -- parse options
  optsParser <- parseOptions
  opts <- execParser $ info (helper <*> optsParser) $
    fullDesc
    <> progDesc "quine"
    <> header "Quine"

  -- set up EKG
  withMonitor opts $ \mon -> do -- start up monitoring
    label "sdl.version" mon >>= \ lv -> version >>= \v -> assign lv $ show v ^. packed
 
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
    window <- createWindow windowName WindowPosUndefined WindowPosUndefined (fromIntegral w) (fromIntegral h) flags

    -- start OpenGL
    cxt <- glCreateContext window
    makeCurrent window cxt
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

    -- System Shutdown Process
    let cleanup = do
          glDeleteContext cxt
          destroyWindow window
          quit
          exitSuccess

    result <- trying _Shutdown $ finally ?? cleanup $ runReaderT ?? System mon opts se $ do
      screenShader <- compile VertexShader   "screen.vert"
      whiteShader  <- compile FragmentShader "white.frag"
      prog <- link screenShader whiteShader
      evalStateT (forever $ poll >> render) $ World prog disp
    either print return result
-- * Rendering

render :: (MonadIO m, MonadState s m, HasWorld s, HasDisplay s) => m ()
render = do
  w <- use displayWindow
  use displayWindowSizeChanged >>= \c -> when c $ do
    sz <- use displayWindowSize
    liftIO $ viewport $= (Position 0 0, sz)
    displayWindowSizeChanged .= False
  liftIO $ do
    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer, StencilBuffer, DepthBuffer]
  use worldProgram >>= \p -> liftIO $ currentProgram $= Just p
  liftIO $ do
    bindVertexArrayObject $= Nothing
    drawArrays Triangles 0 3 -- scene shader is attributeless
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
  liftIO $ hPutStrLn stderr "size changed"
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
event e = liftIO $ hPrint stderr e
