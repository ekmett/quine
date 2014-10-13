module Main where

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
import Data.Foldable
--import Data.Traversable
import Foreign
import Foreign.C
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw as GL
import Graphics.UI.SDL.Basic as SDL
import Graphics.UI.SDL.Enum  as SDL
--import Graphics.UI.SDL.Event as SDL
--import Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Video as SDL


screenWidth, screenHeight :: CInt
screenWidth  = 1400
screenHeight = 1050

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
  alloca $ \windowPointer -> alloca $ \rendererPointer -> do
    putStr "SDL2 Revision: " >> getRevision >>= (peekCString >=> putStrLn)
    _ <- SDL.init initFlagEverything
    _ <- glSetAttribute glAttrContextMajorVersion 4
    _ <- glSetAttribute glAttrContextMinorVersion 1
    _ <- glSetAttribute glAttrContextProfileMask glProfileCore
    _ <- createWindowAndRenderer screenWidth screenHeight 0 windowPointer rendererPointer
    window <- peek windowPointer
    withCString "engine" $ SDL.setWindowTitle window
 
    clearColor $= Color4 0 0 0 1 <?> "set clearColor"
    glEnable gl_FRAMEBUFFER_SRGB <?> "enable sRGB framebuffer"
    quit <?> "quit"
