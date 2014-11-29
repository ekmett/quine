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
import Graphics.GL.Core41
import Quine.GL.Pixel
import Quine.GL.Texture

-- * 2D Image formats

class Storable (PixelBaseComponent a) => ImageFormat a where
  internalFormat  :: p a -> InternalFormat
  pixelFormat     :: p a -> PixelFormat
  pixelType       :: p a -> PixelType
  swizzle         :: p a -> TextureTarget -> IO ()
  swizzle _ _ = return ()

instance ImageFormat PixelRGB8 where
  internalFormat  _ = GL_RGB8
  pixelFormat     _ = GL_RGB
  pixelType       _ = GL_UNSIGNED_BYTE

instance ImageFormat PixelRGB16 where
  internalFormat  _ = GL_RGB16
  pixelFormat     _ = GL_RGB
  pixelType       _ = GL_UNSIGNED_SHORT

instance ImageFormat PixelRGBA8 where
  internalFormat  _ = GL_RGBA8
  pixelFormat     _ = GL_RGBA
  pixelType       _ = GL_UNSIGNED_BYTE

instance ImageFormat PixelRGBA16 where
  internalFormat  _ = GL_RGBA16
  pixelFormat     _ = GL_RGBA
  pixelType       _ = GL_UNSIGNED_SHORT

instance ImageFormat PixelRGBF where
  internalFormat _ = GL_RGB32F
  pixelFormat    _ = GL_RGB
  pixelType      _ = GL_FLOAT

swizzleL :: TextureTarget -> IO ()
swizzleL (TextureTarget t _) = allocaArray 4 $ \p -> do
  pokeArray p [GL_RED, GL_RED, GL_RED, GL_ONE]
  glTexParameteriv t GL_TEXTURE_SWIZZLE_RGBA p

swizzleLA :: TextureTarget -> IO ()
swizzleLA (TextureTarget t _) = allocaArray 4 $ \p -> do
  pokeArray p [GL_RED, GL_RED, GL_RED, GL_GREEN]
  glTexParameteriv t GL_TEXTURE_SWIZZLE_RGBA p

instance ImageFormat Word8 where
  internalFormat _ = GL_R8
  pixelFormat    _ = GL_RED
  pixelType      _ = GL_UNSIGNED_BYTE
  swizzle        _ = swizzleL

instance ImageFormat Word16 where
  internalFormat _ = GL_R16
  pixelFormat    _ = GL_RED
  pixelType      _ = GL_UNSIGNED_SHORT
  swizzle        _ = swizzleL

instance ImageFormat Float where
  internalFormat _ = GL_R32F
  pixelFormat    _ = GL_RED
  pixelType      _ = GL_FLOAT
  swizzle        _ = swizzleL

instance ImageFormat PixelYA8 where
  internalFormat _ = GL_RG8
  pixelFormat    _ = GL_RG
  pixelType      _ = GL_UNSIGNED_BYTE
  swizzle        _ = swizzleLA

instance ImageFormat PixelYA16 where
  internalFormat _ = GL_RG16
  pixelFormat    _ = GL_RG
  pixelType      _ = GL_UNSIGNED_SHORT
  swizzle        _ = swizzleLA

-- * Download

-- @'download' x y w h@ copies w*h region of the screen starting from position (x,y) into a JuicyPixels image
download :: forall m a. (MonadIO m, ImageFormat a) => Int -> Int -> Int -> Int -> m (Image a)
download x y w h
  | w >= 0, h >= 0, n <- w * h = liftIO $ do
    fp <- mallocForeignPtrArray (compSize2D 1 fmt typ w h)
    glPixelStorei GL_UNPACK_LSB_FIRST    0
    glPixelStorei GL_UNPACK_SWAP_BYTES   0
    glPixelStorei GL_UNPACK_ROW_LENGTH   0
    glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
    glPixelStorei GL_UNPACK_SKIP_ROWS    0
    glPixelStorei GL_UNPACK_SKIP_PIXELS  0
    glPixelStorei GL_UNPACK_SKIP_IMAGES  0
    glPixelStorei GL_UNPACK_ALIGNMENT    1 -- normally 4!
    withForeignPtr fp $ glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) fmt typ . castPtr
    return $ Image w h $ V.unsafeFromForeignPtr fp 0 n
  | otherwise = error "download: bad size"
  where fmt = pixelFormat (Proxy :: Proxy a)
        typ = pixelType   (Proxy :: Proxy a)

-- | @'downloadM' x0 y0@ copies the screen starting at position (x,y) into an existing mutable image
downloadM :: forall m a. (MonadIO m, ImageFormat a) => Int -> Int -> MutableImage RealWorld a -> m ()
downloadM x y (MutableImage w h mv) = liftIO $ do
    glPixelStorei GL_UNPACK_LSB_FIRST    0
    glPixelStorei GL_UNPACK_SWAP_BYTES   0
    glPixelStorei GL_UNPACK_ROW_LENGTH   0
    glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
    glPixelStorei GL_UNPACK_SKIP_ROWS    0
    glPixelStorei GL_UNPACK_SKIP_PIXELS  0
    glPixelStorei GL_UNPACK_SKIP_IMAGES  0
    glPixelStorei GL_UNPACK_ALIGNMENT    1 -- normally 4!
    MV.unsafeWith mv $ glReadPixels
      (fromIntegral x) (fromIntegral y)
      (fromIntegral w) (fromIntegral h)
      (pixelFormat (Proxy :: Proxy a))
      (pixelType   (Proxy :: Proxy a)) . castPtr

-- * Upload

class Image2D i where
  upload :: MonadIO m => i -> TextureTarget -> MipmapLevel -> m ()

instance ImageFormat a => Image2D (Image a) where
  upload i@(Image w h v) ta@(TextureTarget t _) l = liftIO $ do
    glPixelStorei GL_PACK_LSB_FIRST    0
    glPixelStorei GL_PACK_SWAP_BYTES   0
    glPixelStorei GL_PACK_ROW_LENGTH   0
    glPixelStorei GL_PACK_IMAGE_HEIGHT 0
    glPixelStorei GL_PACK_SKIP_ROWS    0
    glPixelStorei GL_PACK_SKIP_PIXELS  0
    glPixelStorei GL_PACK_SKIP_IMAGES  0
    glPixelStorei GL_PACK_ALIGNMENT    1 -- normally 4!
    V.unsafeWith v $ glTexImage2D t l (internalFormat i) (fromIntegral w) (fromIntegral h) 0 (pixelFormat i) (pixelType i) . castPtr
    swizzle i ta

instance (ImageFormat a, s ~ RealWorld) => Image2D (MutableImage s a) where
  upload i@(MutableImage w h v) ta@(TextureTarget t _) l = liftIO $ do
    glPixelStorei GL_PACK_LSB_FIRST    0
    glPixelStorei GL_PACK_SWAP_BYTES   0
    glPixelStorei GL_PACK_ROW_LENGTH   0
    glPixelStorei GL_PACK_IMAGE_HEIGHT 0
    glPixelStorei GL_PACK_SKIP_ROWS    0
    glPixelStorei GL_PACK_SKIP_PIXELS  0
    glPixelStorei GL_PACK_SKIP_IMAGES  0
    glPixelStorei GL_PACK_ALIGNMENT    1 -- normally 4!
    MV.unsafeWith v $ glTexImage2D t l (internalFormat i) (fromIntegral w) (fromIntegral h) 0 (pixelFormat i) (pixelType i) . castPtr
    swizzle i ta

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
