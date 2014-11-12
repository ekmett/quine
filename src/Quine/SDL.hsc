{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Prettier SDL bindings
--------------------------------------------------------------------
module Quine.SDL
  ( 
  -- * Versioning
    version
  , revision
  , revisionNumber
  -- * Attribute StateVars
  , contextMajorVersion
  , contextMinorVersion
  , contextFlags
  , contextProfileMask
  , redSize
  , greenSize
  , blueSize
  , alphaSize
  , bufferSize
  , depthSize
  , stencilSize
  , accumRedSize
  , accumGreenSize
  , accumBlueSize
  , accumAlphaSize
  , multiSampleBuffers
  , multiSampleSamples
  , stereo
  , acceleratedVisual
  , doubleBuffer
  , shareWithCurrentContext
  , framebufferSRGBCapable
  , swapInterval
  , windowDisplayMode
  , desktopDisplayMode
  , windowSize
  , makeCurrent
  , relativeMouseMode
  -- * Extensible Exceptions
  , SDLException(..)
  -- * Utilities
  , poll
  , err
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Data.Typeable
import Data.Version as Data
import Foreign
import Foreign.C
import Quine.StateVar
import Graphics.UI.SDL as SDL
import Prelude hiding (init)
import System.IO.Unsafe

#include "SDL.h"

-- | This is thrown in the event of an error in the @Quine.SDL@ combinators
newtype SDLException = SDLException String
  deriving (Show, Typeable)

instance Exception SDLException

-- | Treat negative return codes as prompting an error check.
err :: MonadIO m => CInt -> m ()
err e 
  | e < 0 = liftIO $ do
    msg <- getError >>= peekCString
    clearError
    when (msg /= "") $ throw $ SDLException msg
  | otherwise = return ()

-- | Get/Set relative mouse mode. When enabled we get relative mouse position events even at the screen edge.
relativeMouseMode :: StateVar Bool
relativeMouseMode = StateVar getRelativeMouseMode (setRelativeMouseMode >=> err)

-- * Version

-- | Get the Version (and Revision)
version :: Data.Version
version = unsafePerformIO $ alloca $ \p -> do
  getVersion p
  SDL.Version x y z <- peek p
  return $ Data.Version (fromIntegral <$> [fromIntegral x,fromIntegral y,fromIntegral z, revisionNumber]) []
{-# NOINLINE version #-}

revision :: String
revision = unsafePerformIO $ getRevision >>= peekCString
{-# NOINLINE revision #-}

revisionNumber :: Int
revisionNumber = unsafePerformIO $ fromIntegral <$> getRevisionNumber
{-# NOINLINE revisionNumber #-}

-- * Attribute StateVars
  
-- | get\/set @SDL_GL_RED_SIZE@, the minimum number of bits for the red channel of the color buffer; defaults to 3
redSize :: StateVar Int
redSize = attr SDL_GL_RED_SIZE

-- | get\/set @SDL_GL_GREEN_SIZE@, the minimum number of bits for the green channel of the color buffer; defaults to 3
greenSize :: StateVar Int
greenSize = attr SDL_GL_GREEN_SIZE

-- | get\/set @SDL_GL_BLUE_SIZE@, the minimum number of bits for the blue channel of the color buffer; defaults to 2
blueSize :: StateVar Int
blueSize = attr SDL_GL_BLUE_SIZE

-- | get\/set @SDL_GL_ALPHA_SIZE@, the minimum number of bits for the alpha channel of the color buffer; defaults to 0
alphaSize :: StateVar Int
alphaSize = attr SDL_GL_ALPHA_SIZE

-- | get\/set @SDL_GL_BUFFER_SIZE@, the minimum number of bits for frame buffer size; defaults to 0
bufferSize :: StateVar Int
bufferSize = attr SDL_GL_BUFFER_SIZE

-- | get\/set @SDL_GL_DEPTH_SIZE@, the minimum number of bits in the depth buffer; defaults to 16
depthSize :: StateVar Int
depthSize = attr SDL_GL_DEPTH_SIZE

-- | get\/set @SDL_GL_STENCIL_SIZE@, the minimum number of bits in the stencil buffer; defaults to 0
stencilSize :: StateVar Int
stencilSize = attr SDL_GL_STENCIL_SIZE

-- | get\/set @SDL_GL_ACCUM_RED_SIZE@, the minimum number of bits for the red channel of the accumulation buffer; defaults to 0
accumRedSize :: StateVar Int
accumRedSize = attr SDL_GL_ACCUM_RED_SIZE

-- | get\/set @SDL_GL_ACCUM_GREEN_SIZE@, the minimum number of bits for the green channel of the accumulation buffer; defaults to 0
accumGreenSize :: StateVar Int
accumGreenSize = attr SDL_GL_ACCUM_GREEN_SIZE

-- | get\/set @SDL_GL_ACCUM_BLUE_SIZE@, the minimum number of bits for the blue channel of the accumulation buffer; defaults to 0
accumBlueSize :: StateVar Int
accumBlueSize = attr SDL_GL_ACCUM_BLUE_SIZE

-- | get\/set @SDL_GL_ACCUM_ALPHA_SIZE@, the minimum number of bits for the alpha channel of the accumulation buffer; defaults to 0
accumAlphaSize :: StateVar Int
accumAlphaSize = attr SDL_GL_ACCUM_ALPHA_SIZE

-- | get\/set @SDL_GL_MULTISAMPLEBUFFERS@, the number of buffers used for multisample anti-aliasing; defaults to 0; see <https://wiki.libsdl.org/SDL_GLattr#multisample Remarks> for details
multiSampleBuffers :: StateVar Int
multiSampleBuffers  = attr SDL_GL_MULTISAMPLEBUFFERS

-- | get\/set @SDL_GL_MULTISAMPLESAMPLES@, the number of samples used around the current pixel used for multisample anti-aliasing; defaults to 0; see <https://wiki.libsdl.org/SDL_GLattr#multisample Remarks> for details
multiSampleSamples :: StateVar Int
multiSampleSamples  = attr SDL_GL_MULTISAMPLESAMPLES

-- | get\/set @SDL_GL_CONTEXT_MAJOR_VERSION@, OpenGL context major version; see <https://wiki.libsdl.org/SDL_GLattr#OpenGL Remarks> for details 
contextMajorVersion :: StateVar Int
contextMajorVersion = attr SDL_GL_CONTEXT_MAJOR_VERSION

-- | get\/set @SDL_GL_CONTEXT_MINOR_VERSION@, OpenGL context major version; see <https://wiki.libsdl.org/SDL_GLattr#OpenGL Remarks> for details 
contextMinorVersion :: StateVar Int
contextMinorVersion = attr SDL_GL_CONTEXT_MINOR_VERSION

-- | get\/set @SDL_GL_CONTEXT_FLAGS@, some bitwise (.|.) of 0 or more of:
--
-- 'glContextFlagDebug'
--
-- This flag maps to @GLX_CONTEXT_DEBUG_BIT_ARB@ in the @GLX_ARB_create_context@ extension for @X11@ and @WGL_CONTEXT_DEBUG_BIT_ARB@ in the @WGL_ARB_create_context@ extension for Windows. This flag is currently ignored on other targets that don't support equivalent functionality. This flag is intended to put the GL into a \"debug\" mode which might offer better developer insights, possibly at a loss of performance (although a given GL implementation may or may not do anything differently in the presence of this flag).
--
-- 'SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG'
--
-- This flag maps to @GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB@ in the @GLX_ARB_create_context@ extension for X11 and @WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB@ in the @WGL_ARB_create_context@ extension for Windows. This flag is currently ignored on other targets that don't support equivalent functionality. This flag is intended to put the GL into a \"forward compatible\" mode, which means that no deprecated functionality will be supported, possibly at a gain in performance, and only applies to GL 3.0 and later contexts.
--
-- 'SDL_GL_CONTEXT_RESET_ISOLATION_FLAG'
--
-- This flag maps to @GLX_CONTEXT_RESET_ISOLATION_BIT_ARB@ in the @GLX_ARB_robustness_isolation@ extension for X11 and @WGL_CONTEXT_RESET_ISOLATION_BIT_ARB@ in the @WGL_ARB_create_context_robustness@ extension for Windows. This flag is currently ignored on other targets that don't support equivalent functionality. This flag is intended to require the GL to make promises about what to do in the face of driver or hardware failure.
--
-- 'SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG'
--
-- This flag maps to @GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB@ in the @GLX_ARB_create_context_robustness@ extension for X11 and @WGL_CONTEXT_ROBUST_ACCESS_BIT_ARB@ in the @WGL_ARB_create_context_robustness@ extension for Windows. This flag is currently ignored on other targets that don't support equivalent functionality. This flag is intended to require a GL context that supports the GL_ARB_robustness extension--a mode that offers a few APIs that are safer than the usual defaults (think @snprintf@() vs @sprintf@()).

contextFlags :: StateVar Int
contextFlags = attr SDL_GL_CONTEXT_FLAGS

-- | get\/set @SDL_GL_CONTEXT_PROFILE_MASK@, which must be _one_ of
--
-- * 'SDL_GL_CONTEXT_PROFILE_COMPATIBILITY'
--
-- * 'SDL_GL_CONTEXT_PROFILE_CORE'
--
-- * 'SDL_GL_CONTEXT_PROFILE_ES'
--
-- Despite the name implying you could (.|.) these together, these are mutually exclusive!

contextProfileMask :: StateVar Int
contextProfileMask  = attr SDL_GL_CONTEXT_PROFILE_MASK

-- | get\/set @SDL_GL_STEREO@, whether the output is stereo 3D; defaults to off
stereo :: StateVar Bool
stereo = boolAttr SDL_GL_STEREO

-- | get\/set @SDL_GL_ACCELERATED_VISUAL@, set to 'True' to require hardware acceleration, set to 'False' to force software rendering; defaults to allow either
acceleratedVisual :: StateVar Bool
acceleratedVisual = boolAttr SDL_GL_ACCELERATED_VISUAL

-- | get\/set @SDL_GL_DOUBLEBUFFER@
doubleBuffer :: StateVar Bool
doubleBuffer = boolAttr SDL_GL_DOUBLEBUFFER

-- | get\/set @SDL_GL_SHARE_WITH_CURRENT_CONTEXT@
shareWithCurrentContext :: StateVar Bool
shareWithCurrentContext = boolAttr SDL_GL_SHARE_WITH_CURRENT_CONTEXT

-- | get\/set @SDL_GL_FRAMEBUFFER_SRGB_CAPABLE@
framebufferSRGBCapable :: StateVar Bool
framebufferSRGBCapable  = boolAttr SDL_GL_FRAMEBUFFER_SRGB_CAPABLE

windowDisplayMode :: Window -> StateVar DisplayMode
windowDisplayMode w = StateVar getWDM setWDM where
  getWDM = alloca $ \p -> do
    getWindowDisplayMode w p >>= err
    peek p 
  setWDM m = alloca $ \p -> do
    poke p m
    setWindowDisplayMode w p >>= err

desktopDisplayMode :: Int -> IO DisplayMode
desktopDisplayMode idx = alloca $ \p -> do
  getDesktopDisplayMode (fromIntegral idx) p >>= err
  peek p

elemOff :: forall a. Storable a => Ptr a -> Int -> Ptr a
elemOff p n = p `plusPtr` (n * sizeOf (undefined :: a))

windowSize :: Window -> StateVar (Int, Int)
windowSize win = StateVar g s where
 g = allocaArray 2 $ \p -> do
   getWindowSize win p (elemOff p 1)
   w <- peek p
   h <- peekElemOff p 1
   return $ (fromIntegral w, fromIntegral h)
 s (w, h) = setWindowSize win (fromIntegral w) (fromIntegral h)

-- | Abstracts over @SDL_GL_GetSwapInterval@ / @SDL_GL_SetSwapInterval@
--
-- 0 for immediate updates, 1 for updates synchronized with the vertical retrace. -1 for late swap tearing (if supported)
-- late swap tearing support can be checked under the @GLX_EXT_swap_control_tear@ extension

swapInterval :: StateVar Int
swapInterval = StateVar (fromIntegral <$> glGetSwapInterval) (\a -> glSetSwapInterval (fromIntegral a) >>= err)

-- * Utilities

-- | Use a GLattr as a variable
attr :: GLattr -> StateVar Int
attr a = StateVar (getAttr a) (setAttr a)

boolAttr :: GLattr -> StateVar Bool
boolAttr = mapStateVar fromEnum toEnum . attr

getAttr :: GLattr -> IO Int
getAttr a = alloca $ \p -> do
 glGetAttribute a p >>= err
 fromIntegral <$> peek p

setAttr :: GLattr -> Int -> IO ()
setAttr a i = glSetAttribute a (fromIntegral i) >>= err

makeCurrent :: MonadIO m => Window -> GLContext -> m ()
makeCurrent w c = glMakeCurrent w c >>= err

-- | Poll events using a given handler repeatedly in a loop
poll :: MonadIO m => (Event -> m ()) -> m ()
poll h = do
  me <- liftIO $ alloca $ \ep -> do
    r <- pollEvent ep
    if r /= 0 then Just <$> peek ep
              else return Nothing
  case me of
    Just e  -> h e >> poll h
    Nothing -> return ()
