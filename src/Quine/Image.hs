{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Transcoding between @JuicyPixels@ and @gl@
--------------------------------------------------------------------
module Quine.Image
  ( 
  -- * Uploading to OpenGL
    Image2D(upload)
  -- * Downloading from OpenGL
  , download, downloadM
  -- * Image formats
  , ImageFormat(..)
  ) where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Proxy
import Data.Vector.Storable as V
import Data.Vector.Storable.Mutable as MV
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.GL.Raw.Profile.Core41
import Graphics.GL.Raw.Types 

-- * 2D Image formats

class Storable (PixelBaseComponent a) => ImageFormat a where
  pixelInternalFormat :: p a -> GLint
  pixelFormat         :: p a -> GLenum
  pixelDataType       :: p a -> GLenum
  pixelSwizzle        :: p a -> GLenum -> IO ()
  pixelSwizzle _ _ = return ()

instance ImageFormat PixelRGB8 where
  pixelInternalFormat _ = GL_RGB8
  pixelFormat         _ = GL_RGB
  pixelDataType       _ = GL_UNSIGNED_BYTE

instance ImageFormat PixelRGB16 where
  pixelInternalFormat _ = GL_RGB16
  pixelFormat         _ = GL_RGB
  pixelDataType       _ = GL_UNSIGNED_SHORT

instance ImageFormat PixelRGBA8 where
  pixelInternalFormat _ = GL_RGBA8
  pixelFormat         _ = GL_RGBA
  pixelDataType       _ = GL_UNSIGNED_BYTE

instance ImageFormat PixelRGBA16 where
  pixelInternalFormat _ = GL_RGBA16
  pixelFormat         _ = GL_RGBA
  pixelDataType       _ = GL_UNSIGNED_SHORT

instance ImageFormat PixelRGBF where
  pixelInternalFormat _ = GL_RGB32F
  pixelFormat         _ = GL_RGB
  pixelDataType       _ = GL_FLOAT

swizzleL :: GLenum -> IO ()
swizzleL t = do
  allocaArray 4 $ \p -> do
    pokeArray p [GL_RED, GL_RED, GL_RED, GL_ONE]
    glTexParameteriv t GL_TEXTURE_SWIZZLE_RGBA p

swizzleLA :: GLenum -> IO ()
swizzleLA t = do
  allocaArray 4 $ \p -> do
    pokeArray p [GL_RED, GL_RED, GL_RED, GL_GREEN]
    glTexParameteriv t GL_TEXTURE_SWIZZLE_RGBA p

instance ImageFormat Word8 where
  pixelInternalFormat _ = GL_R8
  pixelFormat         _ = GL_RED
  pixelDataType       _ = GL_UNSIGNED_BYTE
  pixelSwizzle        _ = swizzleL

instance ImageFormat Word16 where
  pixelInternalFormat _ = GL_R16
  pixelFormat         _ = GL_RED
  pixelDataType       _ = GL_UNSIGNED_SHORT
  pixelSwizzle        _ = swizzleL

instance ImageFormat Float where
  pixelInternalFormat _ = GL_R32F
  pixelFormat         _ = GL_RED
  pixelDataType       _ = GL_FLOAT
  pixelSwizzle        _ = swizzleL

instance ImageFormat PixelYA8 where
  pixelInternalFormat _ = GL_RG8
  pixelFormat         _ = GL_RG
  pixelDataType       _ = GL_UNSIGNED_BYTE
  pixelSwizzle        _ = swizzleLA

instance ImageFormat PixelYA16 where
  pixelInternalFormat _ = GL_RG16
  pixelFormat         _ = GL_RG
  pixelDataType       _ = GL_UNSIGNED_SHORT
  pixelSwizzle        _ = swizzleLA

-- * Download

-- @'download' x y w h@ copies w*h region of the screen starting from position (x,y) into a JuicyPixels image
download :: forall m a. (MonadIO m, ImageFormat a) => Int -> Int -> Int -> Int -> m (Image a)
download x y w h
  | w >= 0, h >= 0, n <- w * h = liftIO $ do
    fp <- mallocForeignPtrArray n
    withForeignPtr fp $ glReadPixels
      (fromIntegral x) (fromIntegral y)
      (fromIntegral w) (fromIntegral h)
      (pixelFormat   (Proxy :: Proxy a))
      (pixelDataType (Proxy :: Proxy a)) . castPtr
    return $ Image w h $ V.unsafeFromForeignPtr fp 0 n
  | otherwise = error "download: bad size"

-- | @'downloadM' x0 y0@ copies the screen starting at position (x,y) into an existing mutable image
downloadM :: forall m a. (MonadIO m, ImageFormat a) => Int -> Int -> MutableImage RealWorld a -> m ()
downloadM x y (MutableImage w h mv) = liftIO $ MV.unsafeWith mv $ glReadPixels
  (fromIntegral x) (fromIntegral y)
  (fromIntegral w) (fromIntegral h)
  (pixelFormat   (Proxy :: Proxy a))
  (pixelDataType (Proxy :: Proxy a)) . castPtr

-- * Upload

class Image2D i where
  upload :: MonadIO m => i -> GLenum -> m ()

instance ImageFormat a => Image2D (Image a) where
  upload i@(Image w h v) t = liftIO $ do
    V.unsafeWith v $ glTexImage2D t 0 (pixelInternalFormat i) (fromIntegral w) (fromIntegral h) 0 (pixelFormat i) (pixelDataType i) . castPtr
    pixelSwizzle i t

instance (ImageFormat a, s ~ RealWorld) => Image2D (MutableImage s a) where
  upload i@(MutableImage w h v) t = liftIO $ do
    MV.unsafeWith v $ glTexImage2D t 0 (pixelInternalFormat i) (fromIntegral w) (fromIntegral h) 0 (pixelFormat i) (pixelDataType i) . castPtr
    pixelSwizzle i t

instance Image2D DynamicImage where
  upload (ImageY8 i)     = upload i
  upload (ImageY16 i)    = upload i
  upload (ImageYF i)     = upload i
  upload (ImageYA8 i)    = upload i
  upload (ImageYA16 i)   = upload i
  upload (ImageRGB8 i)   = upload i
  upload (ImageRGB16 i)  = upload i
  upload (ImageRGBF i)   = upload i
  upload (ImageRGBA8 i)  = upload i
  upload (ImageRGBA16 i) = upload i
  upload (ImageYCbCr8 i) = upload (convertImage i :: Image PixelRGB8)
  upload (ImageCMYK8 i)  = upload (convertImage i :: Image PixelRGB8)
  upload (ImageCMYK16 i) = upload (convertImage i :: Image PixelRGB16)
