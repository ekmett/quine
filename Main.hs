{-# LANGUAGE TemplateHaskell, PatternSynonyms #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad hiding (forM_)
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Data.Monoid
import Engine.Display
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

data World = World { _worldProgram :: !Program, _worldDisplay :: !Display }

makeClassy ''World

instance HasDisplay World where
  display = worldDisplay

main :: IO ()
main = runInBoundThread $ withCString "engine" $ \windowName -> do
  optsParser <- parseOptions

  opts <- execParser $ info (helper <*> optsParser) $
    fullDesc
    <> progDesc "engine"
    <> header "Engine"

  ver <- version
  putStrLn $ "SDL2 " ++ show ver

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
  runReaderT ?? se $ do
    poll <- buildPoll
    screenShader <- compile VertexShader   "screen.vert"
    whiteShader  <- compile FragmentShader "white.frag"
    prog <- link screenShader whiteShader
    evalStateT (forever $ poll >> render) $ World prog disp

render :: (MonadIO m, MonadState s m, HasDisplay s) => m ()
render = do
  w <- use displayWindow
  liftIO $ do
    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer, StencilBuffer, DepthBuffer]
    glSwapWindow w

shutdown :: MonadIO m => m ()
shutdown = liftIO $ quit >> exitSuccess

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
-- escape
handleEvent KeyboardEvent{eventType = EventTypeKeyDown, keyboardEventKeysym=Keysym{keysymKeycode = k, keysymMod = m }}
  | m .&. (KeymodRGUI .|. KeymodLGUI) /= 0 = guiKey k
handleEvent e = liftIO $ hPrint stderr e
