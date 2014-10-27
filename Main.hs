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
    let flags = WindowFlagOpenGL
            .|. WindowFlagShown
            .|. WindowFlagResizable
            .|. (if opts^.optionsHighDPI then WindowFlagAllowHighDPI else 0)
            .|. (if opts^.optionsFullScreen then WindowFlagFullscreen else 0)
    window <- createWindow windowName WindowPosUndefined WindowPosUndefined (fromIntegral $ opts^.optionsWindowWidth) (fromIntegral $ opts^.optionsWindowHeight) flags
    cxt <- glCreateContext window
    glEnable gl_FRAMEBUFFER_SRGB
    se <- buildShaderEnv opts
    let disp = Display window cxt (opts^.optionsFullScreen) def
    runReaderT ?? System mon opts se $ do
      poll <- buildPoll
      screenShader <- compile VertexShader   "screen.vert"
      whiteShader  <- compile FragmentShader "white.frag"
      prog <- link screenShader whiteShader
      evalStateT ?? World prog disp $ forever (poll >> render)
  
-- * Rendering

render :: (MonadIO m, MonadState s m, HasDisplay s) => m ()
render = do
  w <- use displayWindow
  liftIO $ do
    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer, StencilBuffer, DepthBuffer]
    glSwapWindow w

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
          handleEvent e
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

handleEvent :: (MonadIO m, MonadState s m, HasDisplay s) => SDL.Event -> m ()
handleEvent QuitEvent{} = shutdown
handleEvent KeyboardEvent{eventType = EventTypeKeyDown, keyboardEventKeysym=Keysym{keysymKeycode = k, keysymMod = m }}
  | m .&. (KeymodRGUI .|. KeymodLGUI) /= 0 = guiKey k
handleEvent e = liftIO $ hPrint stderr e
