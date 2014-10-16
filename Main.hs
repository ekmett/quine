module Main where

import Control.Monad hiding (forM_)
import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
import Data.Foldable
--import Data.Traversable
import Foreign
import Foreign.C
import System.Exit
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw as GL
import Graphics.UI.SDL.Basic as SDL
import Graphics.UI.SDL.Enum  as SDL
import Graphics.UI.SDL.Event as SDL
import Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Video as SDL

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
  putStr "SDL2 Revision: " >> getRevision >>= (peekCString >=> putStrLn)
  _ <- SDL.init initFlagVideo -- Everything
  -- request some minimums
  _ <- glSetAttribute glAttrContextMajorVersion 4
  _ <- glSetAttribute glAttrContextMinorVersion 1
  _ <- glSetAttribute glAttrRedSize 5
  _ <- glSetAttribute glAttrGreenSize 5
  _ <- glSetAttribute glAttrBlueSize 5
  _ <- glSetAttribute glAttrDepthSize 16
  _ <- glSetAttribute glAttrDoubleBuffer 1

  _ <- glSetAttribute glAttrContextProfileMask glProfileCore
  window <- createWindow windowName windowPosUndefined windowPosUndefined 1024 768 (windowFlagOpenGL .|. windowFlagShown .|. windowFlagResizable .|. windowFlagAllowHighDPI)
  _ <- glCreateContext window
  alloca $ \iptr -> do
    red   <- glGetAttribute glAttrRedSize   iptr >> peek iptr
    green <- glGetAttribute glAttrGreenSize iptr >> peek iptr
    blue  <- glGetAttribute glAttrBlueSize  iptr  >> peek iptr
    print ("red", red, "green", green, "blue", blue)

  glEnable gl_FRAMEBUFFER_SRGB
  forever $ poll >> render

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
