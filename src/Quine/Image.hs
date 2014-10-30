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
-- Transcoding between JuicyPixels and OpenGL
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
import Graphics.Rendering.OpenGL hiding (imageHeight, Proxy)

-- * 2D Image formats

class Storable (PixelBaseComponent a) => ImageFormat a where
  pixelInternalFormat :: p a -> PixelInternalFormat
  pixelFormat         :: p a -> PixelFormat
  pixelDataType       :: p a -> DataType

instance ImageFormat PixelRGB8 where
  pixelInternalFormat _ = RGB'
  pixelFormat         _ = RGB
  pixelDataType       _ = UnsignedByte

instance ImageFormat PixelRGB16 where
  pixelInternalFormat _ = RGB16
  pixelFormat         _ = RGB
  pixelDataType       _ = UnsignedShort

instance ImageFormat PixelRGBA8 where
  pixelInternalFormat _ = RGBA'
  pixelFormat         _ = RGBA
  pixelDataType       _ = UnsignedByte

instance ImageFormat PixelRGBA16 where
  pixelInternalFormat _ = RGBA16
  pixelFormat         _ = RGBA
  pixelDataType       _ = UnsignedShort

instance ImageFormat PixelRGBF where
  pixelInternalFormat _ = RGB32F
  pixelFormat         _ = RGB
  pixelDataType       _ = Float

instance ImageFormat Word8 where
  pixelInternalFormat _ = Luminance8
  pixelFormat         _ = Luminance
  pixelDataType       _ = UnsignedByte

instance ImageFormat Word16 where
  pixelInternalFormat _ = Luminance16
  pixelFormat         _ = Luminance
  pixelDataType       _ = UnsignedShort

instance ImageFormat Word32 where
  pixelInternalFormat _ = R32I
  pixelFormat         _ = Red
  pixelDataType       _ = UnsignedInt

-- | Luminance32F is missing, you'll be seeing Red
--
-- <https://github.com/haskell-opengl/OpenGL/issues/66>
instance ImageFormat Float where
  pixelInternalFormat _ = R32F
  pixelFormat         _ = Red
  pixelDataType       _ = Float

instance ImageFormat PixelYA8 where
  pixelInternalFormat _ = Luminance8Alpha8
  pixelFormat         _ = LuminanceAlpha
  pixelDataType       _ = UnsignedByte

instance ImageFormat PixelYA16 where
  pixelInternalFormat _ = Luminance16Alpha16
  pixelFormat         _ = LuminanceAlpha
  pixelDataType       _ = UnsignedShort

-- * Download

download :: forall m a. (MonadIO m, ImageFormat a) => Position -> Size -> m (Image a)
download pos sz@(Size w0 h0) 
  | w <- fromIntegral w0, w >= 0
  , h <- fromIntegral h0, h >= 0
  , n <- w * h = liftIO $ do
    fp <- mallocForeignPtrArray n
    withForeignPtr fp $ \ p -> readPixels pos sz $
      PixelData (pixelFormat (Proxy :: Proxy a)) (pixelDataType (Proxy :: Proxy a)) p
    return $ Image w h $ V.unsafeFromForeignPtr fp 0 n
  | otherwise = error "download: bad size"

downloadM :: forall m a. (MonadIO m, ImageFormat a) => Position -> MutableImage RealWorld a -> m ()
downloadM pos (MutableImage w h mv) = liftIO $ MV.unsafeWith mv $ \ p -> readPixels pos (Size (fromIntegral w) (fromIntegral h)) $
    PixelData (pixelFormat (Proxy :: Proxy a)) (pixelDataType (Proxy :: Proxy a)) p

-- * Upload

class Image2D i where
  upload :: (MonadIO m, TwoDimensionalTextureTarget t) => i -> t -> m ()

instance ImageFormat a => Image2D (Image a) where
  upload i@(Image w h v) t = liftIO $ V.unsafeWith v $ \p -> 
    texImage2D 
      t 
      NoProxy 
      0 -- level
      (pixelInternalFormat i) 
      (TextureSize2D (fromIntegral w) (fromIntegral h))
      0 -- border
      (PixelData (pixelFormat i) (pixelDataType i) p)

instance (ImageFormat a, s ~ RealWorld) => Image2D (MutableImage s a) where
  upload i@(MutableImage w h v) t = liftIO $ MV.unsafeWith v $ \p -> 
    texImage2D 
      t 
      NoProxy 
      0 -- level
      (pixelInternalFormat i) 
      (TextureSize2D (fromIntegral w) (fromIntegral h))
      0 -- border
      (PixelData (pixelFormat i) (pixelDataType i) p)

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
