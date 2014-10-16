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
  liftIO $ putStrLn xs
  a <- m -- todo wrap this with a try
  es <- liftIO (get errors)
  unless (null es) $ do
    -- liftIO $ putStrLn xs
    forM_ es $ liftIO . print
  return a

main :: IO ()
main =
  withCString "engine" $ \windowName -> do
    putStr "SDL2 Revision: " >> getRevision >>= (peekCString >=> putStrLn)
    _ <- SDL.init initFlagEverything
    _ <- glSetAttribute glAttrContextMajorVersion 4
    _ <- glSetAttribute glAttrContextMinorVersion 1
    _ <- glSetAttribute glAttrContextProfileMask glProfileCore
    window <- createWindow windowName windowPosUndefined windowPosUndefined 1024 768 (windowFlagOpenGL .|. windowFlagShown .|. windowFlagResizable .|. windowFlagAllowHighDPI)
    _ <- glCreateContext window
    clearColor $= Color4 0 0 0 1
    glEnable gl_FRAMEBUFFER_SRGB
    forever $ do
      pollEvents handleEvent
      render

render :: IO ()
render = do
  clearColor $= Color4 0 0 0 1

handleEvent :: SDL.Event -> IO ()
handleEvent QuitEvent{}                                                    = shutdown
handleEvent KeyboardEvent{keyboardEventKeysym=Keysym{keysymKeycode = 27 }} = shutdown
handleEvent e = print e

shutdown :: IO ()
shutdown = quit >> exitSuccess

pollEvents :: (SDL.Event -> IO ()) -> IO ()
pollEvents k = alloca go where
  go ep = pollEvent ep >>= \ r -> when (r /= 0) $ do
    peek ep >>= k
    go ep

{-
loop =
  do quit <- whileEvents
     unless quit loop

whileEvents =
  do event <- pollEvent
     case event of
       Quit -> return True
       NoEvent -> return False
       _ -> whileEvents
-}

