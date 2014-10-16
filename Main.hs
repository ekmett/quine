module Main where

import Control.Monad hiding (forM_)
import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
import Data.Foldable
--import Data.Traversable
import Engine.SDL as Nice
import Foreign
import Foreign.C
import System.Exit
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw as GL
-- import qualified Graphics.UI.SDL.Basic as SDL
import Graphics.UI.SDL.Enum  as SDL
import Graphics.UI.SDL.Event as SDL
import Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Video as SDL
import Prelude hiding (init)

screenWidth, screenHeight :: CInt
screenWidth  = 1024
screenHeight = 768

infixl 0 <?>
(<?>) :: MonadIO m => m a -> String -> m a
m <?> xs = do
  a <- m -- todo wrap this with a try
  es <- liftIO (get errors)
  unless (null es) $ do
    liftIO $ putStrLn xs
    forM_ es $ liftIO . print
  return a

main :: IO ()
main = withCString "engine" $ \windowName -> do
  rev <- getRevision
  putStr $ "SDL2 Revision: " ++ rev
  init initFlagVideo
  contextMajorVersion $= 4
  contextMinorVersion $= 1
  redSize   $= 5
  greenSize $= 5
  blueSize  $= 5
  depthSize $= 16
  Nice.doubleBuffer $= True
  _ <- contextProfileMask $= SDL.glProfileCore
  window <- createWindow windowName windowPosUndefined windowPosUndefined 1024 768 (windowFlagOpenGL .|. windowFlagShown .|. windowFlagResizable .|. windowFlagAllowHighDPI)
  _ <- glCreateContext window
  glEnable gl_FRAMEBUFFER_SRGB
  forever (poll >> render)

render :: IO ()
render = do
  clearColor $= Color4 0 0 0 1

shutdown :: IO ()
shutdown = quit >> exitSuccess

poll :: IO ()
poll = alloca go where
  go ep = pollEvent ep >>= \ r -> when (r /= 0) $ do
    peek ep >>= handleEvent
    go ep

handleEvent :: SDL.Event -> IO ()
handleEvent QuitEvent{}                                                    = shutdown
handleEvent KeyboardEvent{keyboardEventKeysym=Keysym{keysymKeycode = 27 }} = shutdown
handleEvent e = print e
