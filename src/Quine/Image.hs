{-# LANGUAGE FlexibleContexts #-}
module Quine.Image
  ( Image2D(..)
  , ImageFormat(..)
  ) where

import Codec.Picture
import Codec.Picture.Types
import Data.Vector.Storable
import Data.Word
import Graphics.Rendering.OpenGL hiding (imageHeight)

class Image2D i where
  image2D :: TwoDimensionalTextureTarget t => i -> t -> IO ()

-- | Transcode formats between JuicyPixels and OpenGL
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

-- | Luminance32F is missing, loads via the Red channel
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


instance ImageFormat a => Image2D (Image a) where
  image2D i t = unsafeWith (imageData i) $ \p -> 
    texImage2D 
      t 
      NoProxy 
      0 -- level
      (pixelInternalFormat i) 
      (TextureSize2D (fromIntegral $ imageWidth i) (fromIntegral $ imageHeight i))
      0 -- border
      (PixelData (pixelFormat i) (pixelDataType i) p)

instance Image2D DynamicImage where
  image2D (ImageY8 i)     = image2D i
  image2D (ImageY16 i)    = image2D i
  image2D (ImageYF i)     = image2D i
  image2D (ImageYA8 i)    = image2D i
  image2D (ImageYA16 i)   = image2D i
  image2D (ImageRGB8 i)   = image2D i
  image2D (ImageRGB16 i)  = image2D i
  image2D (ImageRGBF i)   = image2D i
  image2D (ImageRGBA8 i)  = image2D i
  image2D (ImageRGBA16 i) = image2D i
  image2D (ImageYCbCr8 i) = image2D (convertImage i :: Image PixelRGB8)
  image2D (ImageCMYK8 i)  = image2D (convertImage i :: Image PixelRGB8)
  image2D (ImageCMYK16 i) = image2D (convertImage i :: Image PixelRGB16)
