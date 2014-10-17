{-# LANGUAGE DeriveDataTypeable #-}
module Engine.SDL.Video
  ( 
  -- * Attribute StateVars
    contextMajorVersion
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
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Engine.SDL.Exception
import Engine.Var
import Foreign
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL (GLattr)
import Prelude hiding (init)
-- import Graphics.Rendering.OpenGL.GL.StateVar

-- * Attribute StateVars
  
-- | get\/set @SDL_GL_RED_SIZE@, the minimum number of bits for the red channel of the color buffer; defaults to 3
redSize :: Varied m => m Int
redSize = attr SDL.glAttrRedSize

-- | get\/set @SDL_GL_GREEN_SIZE@, the minimum number of bits for the green channel of the color buffer; defaults to 3
greenSize :: Varied m => m Int
greenSize = attr SDL.glAttrGreenSize

-- | get\/set @SDL_GL_BLUE_SIZE@, the minimum number of bits for the blue channel of the color buffer; defaults to 2
blueSize :: Varied m => m Int
blueSize = attr SDL.glAttrBlueSize

-- | get\/set @SDL_GL_ALPHA_SIZE@, the minimum number of bits for the alpha channel of the color buffer; defaults to 0
alphaSize :: Varied m => m Int
alphaSize = attr SDL.glAttrAlphaSize

-- | get\/set @SDL_GL_BUFFER_SIZE@, the minimum number of bits for frame buffer size; defaults to 0
bufferSize :: Varied m => m Int
bufferSize = attr SDL.glAttrBufferSize

-- | get\/set @SDL_GL_DEPTH_SIZE@, the minimum number of bits in the depth buffer; defaults to 16
depthSize :: Varied m => m Int
depthSize = attr SDL.glAttrDepthSize

-- | get\/set @SDL_GL_STENCIL_SIZE@, the minimum number of bits in the stencil buffer; defaults to 0
stencilSize :: Varied m => m Int
stencilSize = attr SDL.glAttrStencilSize

-- | get\/set @SDL_GL_ACCUM_RED_SIZE@, the minimum number of bits for the red channel of the accumulation buffer; defaults to 0
accumRedSize :: Varied m => m Int
accumRedSize = attr SDL.glAttrAccumRedSize

-- | get\/set @SDL_GL_ACCUM_GREEN_SIZE@, the minimum number of bits for the green channel of the accumulation buffer; defaults to 0
accumGreenSize :: Varied m => m Int
accumGreenSize = attr SDL.glAttrAccumGreenSize

-- | get\/set @SDL_GL_ACCUM_BLUE_SIZE@, the minimum number of bits for the blue channel of the accumulation buffer; defaults to 0
accumBlueSize :: Varied m => m Int
accumBlueSize = attr SDL.glAttrAccumBlueSize

-- | get\/set @SDL_GL_ACCUM_ALPHA_SIZE@, the minimum number of bits for the alpha channel of the accumulation buffer; defaults to 0
accumAlphaSize :: Varied m => m Int
accumAlphaSize = attr SDL.glAttrAccumAlphaSize

-- | get\/set @SDL_GL_MULTISAMPLEBUFFERS@, the number of buffers used for multisample anti-aliasing; defaults to 0; see <https://wiki.libsdl.org/SDL_GLattr#multisample Remarks> for details
multiSampleBuffers :: Varied m => m Int
multiSampleBuffers  = attr SDL.glAttrMultiSampleBuffers

-- | get\/set @SDL_GL_MULTISAMPLESAMPLES@, the number of samples used around the current pixel used for multisample anti-aliasing; defaults to 0; see <https://wiki.libsdl.org/SDL_GLattr#multisample Remarks> for details
multiSampleSamples :: Varied m => m Int
multiSampleSamples  = attr SDL.glAttrMultiSampleSamples

-- | get\/set @SDL_GL_CONTEXT_MAJOR_VERSION@, OpenGL context major version; see <https://wiki.libsdl.org/SDL_GLattr#OpenGL Remarks> for details 
contextMajorVersion :: Varied m => m Int
contextMajorVersion = attr SDL.glAttrContextMajorVersion

-- | get\/set @SDL_GL_CONTEXT_MINOR_VERSION@, OpenGL context major version; see <https://wiki.libsdl.org/SDL_GLattr#OpenGL Remarks> for details 
contextMinorVersion :: Varied m => m Int
contextMinorVersion = attr SDL.glAttrContextMinorVersion

-- | get\/set @SDL_GL_CONTEXT_FLAGS@, some bitwise (.|.) of 0 or more of:
--
-- 'glContextFlagDebug'
--
-- This flag maps to @GLX_CONTEXT_DEBUG_BIT_ARB@ in the @GLX_ARB_create_context@ extension for @X11@ and @WGL_CONTEXT_DEBUG_BIT_ARB@ in the @WGL_ARB_create_context@ extension for Windows. This flag is currently ignored on other targets that don't support equivalent functionality. This flag is intended to put the GL into a \"debug\" mode which might offer better developer insights, possibly at a loss of performance (although a given GL implementation may or may not do anything differently in the presence of this flag).
--
-- 'glContextFlagForwardCompatible'
--
-- This flag maps to @GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB@ in the @GLX_ARB_create_context@ extension for X11 and @WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB@ in the @WGL_ARB_create_context@ extension for Windows. This flag is currently ignored on other targets that don't support equivalent functionality. This flag is intended to put the GL into a \"forward compatible\" mode, which means that no deprecated functionality will be supported, possibly at a gain in performance, and only applies to GL 3.0 and later contexts.
--
-- 'glContextFlagResetIsolation'
--
-- This flag maps to @GLX_CONTEXT_RESET_ISOLATION_BIT_ARB@ in the @GLX_ARB_robustness_isolation@ extension for X11 and @WGL_CONTEXT_RESET_ISOLATION_BIT_ARB@ in the @WGL_ARB_create_context_robustness@ extension for Windows. This flag is currently ignored on other targets that don't support equivalent functionality. This flag is intended to require the GL to make promises about what to do in the face of driver or hardware failure.
--
-- 'glContextFlagRobustAccess'
--
-- This flag maps to @GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB@ in the @GLX_ARB_create_context_robustness@ extension for X11 and @WGL_CONTEXT_ROBUST_ACCESS_BIT_ARB@ in the @WGL_ARB_create_context_robustness@ extension for Windows. This flag is currently ignored on other targets that don't support equivalent functionality. This flag is intended to require a GL context that supports the GL_ARB_robustness extension--a mode that offers a few APIs that are safer than the usual defaults (think @snprintf@() vs @sprintf@()).

contextFlags :: Varied m => m Int
contextFlags = attr SDL.glAttrContextFlags

-- | get\/set @SDL_GL_CONTEXT_PROFILE_MASK@, which must be _one_ of
--
-- * 'glProfileCore'
--
-- * 'glProfileCompatibility'
--
-- * 'glProfileES'
--
-- Despite the name implying you could (.|.) these together, these are mutually exclusive!

contextProfileMask :: Varied m => m Int
contextProfileMask  = attr SDL.glAttrContextProfileMask

-- | get\/set @SDL_GL_STEREO@, whether the output is stereo 3D; defaults to off
stereo :: Varied m => m Bool
stereo = boolAttr SDL.glAttrStereo

-- | get\/set @SDL_GL_ACCELERATED_VISUAL@, set to 'True' to require hardware acceleration, set to 'False' to force software rendering; defaults to allow either
acceleratedVisual :: Varied m => m Bool
acceleratedVisual = boolAttr SDL.glAttrAcceleratedVisual

-- | get\/set @SDL_GL_DOUBLEBUFFER@
doubleBuffer :: Varied m => m Bool
doubleBuffer = boolAttr SDL.glAttrDoubleBuffer

-- | get\/set @SDL_GL_SHARE_WITH_CURRENT_CONTEXT@
shareWithCurrentContext :: Varied m => m Bool
shareWithCurrentContext = boolAttr SDL.glAttrShareWithCurrentContext

-- | get\/set @SDL_GL_FRAMEBUFFER_SRGB_CAPABLE@
framebufferSRGBCapable :: Varied m => m Bool
framebufferSRGBCapable  = boolAttr SDL.glAttrFramebufferSRGBCapable

-- | Abstracts over @SDL_GL_GetSwapInterval@ / @SDL_GL_SetSwapInterval@
--
-- 0 for immediate updates, 1 for updates synchronized with the vertical retrace. -1 for late swap tearing (if supported)
-- late swap tearing support can be checked under the @GLX_EXT_swap_control_tear@ extension

swapInterval :: Varied m => m Int
swapInterval = vary (fromIntegral <$> SDL.glGetSwapInterval) (\a -> SDL.glSetSwapInterval (fromIntegral a) >>= err)

-- * Utilities

-- | Use a GLattr as a variable
attr :: Varied m => GLattr -> m Int
attr a = vary (getAttr a) (setAttr a)

boolAttr :: Varied m => GLattr -> m Bool
boolAttr = xmap fromEnum toEnum . attr

getAttr :: MonadIO m => GLattr -> m Int
getAttr a = liftIO $ alloca $ \p -> do
 SDL.glGetAttribute a p >>= err
 fromIntegral <$> peek p

setAttr :: MonadIO m => GLattr -> Int -> m ()
setAttr a i = liftIO $ SDL.glSetAttribute a (fromIntegral i) >>= err
