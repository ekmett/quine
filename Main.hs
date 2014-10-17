{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad hiding (forM_)
import Control.Monad.Random
import Control.Monad.State
import Engine.SDL.Basic
import Engine.SDL.Video
import Engine.Var
import Foreign
import Foreign.C
import System.Exit
import Graphics.Rendering.OpenGL as GL hiding (doubleBuffer)
import Graphics.Rendering.OpenGL.Raw as GL
import Graphics.UI.SDL.Enum  as SDL
import Graphics.UI.SDL.Event as SDL
import Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Video as SDL
import Prelude hiding (init)

data Config = Config { _configFullScreen :: !Bool, _configWindow :: Window }

makeClassy ''Config

main :: IO ()
main = withCString "engine" $ \windowName -> do
  ver <- version
  putStrLn $ "SDL2 " ++ show ver
  init initFlagEverything
  contextMajorVersion &= 4
  contextMinorVersion &= 1
  contextProfileMask  &= glProfileCore
  redSize   &= 5
  greenSize &= 5
  blueSize  &= 5
  depthSize &= 16
  doubleBuffer &= True
  window <- createWindow windowName windowPosUndefined windowPosUndefined 1024 768 (windowFlagOpenGL .|. windowFlagShown .|. windowFlagResizable .|. windowFlagAllowHighDPI)
  _ <- glCreateContext window
  glEnable gl_FRAMEBUFFER_SRGB
  () <$ execStateT (forever $ poll >> render) (Config False window)

render :: (MonadIO m, MonadState s m, HasConfig s) => m ()
render = do
  w <- use configWindow
  liftIO $ do
    r <- randomIO
    clearColor $= Color4 r 0 0 1
    clear [ColorBuffer, StencilBuffer, DepthBuffer]
    glSwapWindow w

shutdown :: MonadIO m => m ()
shutdown = liftIO $ quit >> exitSuccess

poll :: HasConfig s => StateT s IO ()
poll = StateT $ \s -> alloca $ \ep -> runStateT (go ep) s where
  go ep = liftIO (pollEvent ep) >>= \ r -> when (r /= 0) $ do
    e <- liftIO (peek ep)
    handleEvent e
    go ep

handleEvent :: HasConfig s => SDL.Event -> StateT s IO ()
handleEvent QuitEvent{} = shutdown
handleEvent KeyboardEvent{keyboardEventKeysym=Keysym{keysymKeycode = 27}} = shutdown
handleEvent KeyboardEvent{eventType = et, keyboardEventKeysym=Keysym{keysymKeycode = 13, keysymMod = m }} 
  | et == eventTypeKeyDown && m .&. (keymodAlt .|. keymodGUI) /= 0 = do
  fs <- configFullScreen <%= not
  w  <- use configWindow
  _ <- liftIO $ setWindowFullscreen w $ if fs then windowFlagFullscreenDesktop else 0
  return ()
handleEvent e = liftIO $ print e
