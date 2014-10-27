{-# LANGUAGE TemplateHaskell, PatternSynonyms #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad hiding (forM_)
import Control.Monad.State
import Data.Default
import Data.Monoid
import Engine.Display
import Engine.Options
import Engine.SDL.Basic
import Engine.SDL.Exception
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
  _ <- execStateT (forever $ poll >> render) $ Display window cxt (opts^.optionsFullScreen) def
  return ()

render :: (MonadIO m, MonadState s m, HasDisplay s) => m ()
render = do
  w <- use displayWindow
  liftIO $ do
    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer, StencilBuffer, DepthBuffer]
    glSwapWindow w

shutdown :: MonadIO m => m ()
shutdown = liftIO $ quit >> exitSuccess

poll :: HasDisplay s => StateT s IO ()
poll = StateT $ \s -> alloca $ \ep -> runStateT (go ep) s where
  go ep = liftIO (pollEvent ep) >>= \ r -> when (r /= 0) $ do
    e <- liftIO (peek ep)
    handleEvent e
    go ep

guiKey :: HasDisplay s => Keycode -> StateT s IO ()
guiKey KeycodeQ = shutdown
guiKey KeycodeReturn = do
  fs <- displayFullScreen <%= not
  w  <- use displayWindow
  _ <- liftIO $ setWindowFullscreen w $ if fs then WindowFlagFullscreenDesktop else 0
  return ()
guiKey e = liftIO $ hPrint stderr $ "Command " ++ show e

handleEvent :: HasDisplay s => SDL.Event -> StateT s IO ()
handleEvent QuitEvent{} = shutdown
-- escape
handleEvent KeyboardEvent{eventType = EventTypeKeyDown, keyboardEventKeysym=Keysym{keysymKeycode = k, keysymMod = m }}
  | m .&. (KeymodRGUI .|. KeymodLGUI) /= 0 = guiKey k
handleEvent e = liftIO $ hPrint stderr e
