{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, PatternSynonyms, GeneralizedNewtypeDeriving #-}
module Quine.GL.Pixel
  ( InternalFormat(..)
  , PixelFormat(..)
  , PixelType(..)
  , compSize1D
  , compSize2D
  , compSize3D
  ) where

import Data.Bits
import Data.Data
import GHC.Generics
import Graphics.GL.Standard21
import Graphics.GL.Core45
import Graphics.GL.Ext
import Graphics.GL.Types

newtype InternalFormat = InternalFormat GLint
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic,Num)

newtype PixelFormat = PixelFormat GLenum
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic,Num)

newtype PixelType = PixelType GLenum
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic,Num)

components :: PixelFormat -> Int
components GL_ABGR_EXT = 4
components GL_ALPHA = 1
components GL_BLUE = 1
components GL_CMYK_EXT= 4
components GL_COLOR_INDEX = 1
components GL_DEPTH_COMPONENT = 1
components GL_GREEN = 1
components GL_LUMINANCE = 1
components GL_LUMINANCE_ALPHA = 2
components GL_RED = 1
components GL_RED_EXT = 1
components GL_RGB = 3
components GL_RGBA = 4
components GL_STENCIL_INDEX = 1
components GL_UNSIGNED_INT = 1
components GL_UNSIGNED_SHORT = 1
components GL_YCRCB_422_SGIX = 3 -- gl_SGIX_ycrcb
components GL_YCRCB_444_SGIX = 3 -- gl_SGIX_ycrcb
components GL_YCRCB_SGIX     = 3 -- gl_SGIX_ycrcba
components GL_YCRCBA_SGIX    = 4 -- gl_SGIX_ycrcba
components GL_YCBCR_MESA     = 3 -- gl_MESA_ycbcr_texture
components _ = 0;

typeSize :: PixelType -> Int
typeSize GL_BYTE   = 1
typeSize GL_FLOAT  = 4
typeSize GL_FLOAT_32_UNSIGNED_INT_24_8_REV = 8
typeSize GL_HALF_FLOAT = 2
typeSize GL_HALF_FLOAT_ARB | gl_ARB_half_float_pixel = 2
typeSize GL_INT    = 4
typeSize GL_INT_10_10_10_2_OES = 4 -- vertex format, not an image format
typeSize GL_SHORT  = 2
typeSize GL_UNSIGNED_BYTE           = 1
typeSize GL_UNSIGNED_BYTE_3_3_2     = 1
typeSize GL_UNSIGNED_BYTE_3_3_2_EXT = 1
typeSize GL_UNSIGNED_BYTE_2_3_3_REV = 1
typeSize GL_UNSIGNED_INT                       = 4
typeSize GL_UNSIGNED_INT_10F_11F_11F_REV       = 4
typeSize GL_UNSIGNED_INT_10F_11F_11F_REV_APPLE = 4 -- gl_APPLE_texture_packed_float
typeSize GL_UNSIGNED_INT_10F_11F_11F_REV_EXT   = 4
typeSize GL_UNSIGNED_INT_2_10_10_10_REV        = 4
typeSize GL_UNSIGNED_INT_2_10_10_10_REV_EXT    = 4
typeSize GL_UNSIGNED_INT_5_9_9_9_REV           = 4
typeSize GL_UNSIGNED_INT_5_9_9_9_REV_APPLE     = 4 -- gl_APPLE_texture_packed_float
typeSize GL_UNSIGNED_INT_5_9_9_9_REV_EXT       = 4 -- gl_EXT_packed_float
typeSize GL_UNSIGNED_INT_10_10_10_2            = 4
typeSize GL_UNSIGNED_INT_10_10_10_2_EXT        = 4
typeSize GL_UNSIGNED_INT_10_10_10_2_OES        = 4 -- gl_OES_vertex_type_10_10_10_2
typeSize GL_UNSIGNED_INT_8_8_8_8               = 4
typeSize GL_UNSIGNED_INT_8_8_8_8_EXT           = 4
typeSize GL_UNSIGNED_INT_8_8_8_8_REV           = 4
typeSize GL_UNSIGNED_INT_8_8_S8_S8_REV_NV      = 4
typeSize GL_UNSIGNED_INT_24_8                  = 4
typeSize GL_UNSIGNED_INT_24_8_EXT              = 4
typeSize GL_UNSIGNED_INT_24_8_NV               = 4
typeSize GL_UNSIGNED_INT_24_8_OES              = 4
typeSize GL_UNSIGNED_INT_S8_S8_8_8_NV          = 4
typeSize GL_UNSIGNED_SHORT                 = 2
typeSize GL_UNSIGNED_SHORT_1_5_5_5_REV     = 2
typeSize GL_UNSIGNED_SHORT_1_5_5_5_REV_EXT = 2
typeSize GL_UNSIGNED_SHORT_4_4_4_4         = 2
typeSize GL_UNSIGNED_SHORT_4_4_4_4_REV     = 2
typeSize GL_UNSIGNED_SHORT_4_4_4_4_REV_EXT = 2
typeSize GL_UNSIGNED_SHORT_4_4_4_4_REV_IMG = 2
typeSize GL_UNSIGNED_SHORT_5_5_5_1_EXT     = 2
typeSize GL_UNSIGNED_SHORT_5_6_5           = 2
typeSize GL_UNSIGNED_SHORT_5_6_5_REV       = 2
typeSize GL_UNSIGNED_SHORT_8_8_APPLE       = 2
typeSize GL_UNSIGNED_SHORT_8_8_MESA        = 2 
typeSize GL_UNSIGNED_SHORT_8_8_REV_APPLE   = 2
typeSize GL_UNSIGNED_SHORT_8_8_REV_MESA    = 2 
typeSize GL_BITMAP                         = 0 -- unsuppored
typeSize _ = 0

compSize :: PixelFormat -> PixelType -> Int
compSize a GL_FLOAT          = components a * 4
compSize a GL_HALF_FLOAT     = components a * 2
compSize a GL_BYTE           = components a
compSize a GL_SHORT          = components a * 2
compSize a GL_UNSIGNED_BYTE  = components a
compSize a GL_UNSIGNED_INT   = components a * 4
compSize a GL_UNSIGNED_SHORT = components a * 2
compSize _ b = typeSize b -- packed format

-- | @'compSize1D' pixelFormat pixelType n@ computes the size of a 1D texture of size @n@ in bytes
compSize1D :: PixelFormat -> PixelType -> Int -> Int
compSize1D _ GL_BITMAP n = unsafeShiftR (n + 7) 3 -- removed in 3.3
compSize1D a b         n = compSize a b * n

-- | @'compSize2D' alignment pixelFormat pixelType w h@ computes the size of a 2D texture of size @w * h@ in bytes
-- where the rows are aligned to a given @alignment@ = { 1, 2, 4, 8 } bytes.
compSize2D :: Int -> PixelFormat -> PixelType -> Int -> Int -> Int
compSize2D align a b w h = (rb + mod (-rb) align) * h where rb = compSize1D a b w

-- | @'compSize3D' alignment pixelFormat pixelType w h d@ computes the size of a 3D texture of size @w * h * d@ in bytes
-- where the rows are aligned to a given @alignment@ = { 1, 2, 4, 8 } bytes.
compSize3D :: Int -> PixelFormat -> PixelType -> Int -> Int -> Int -> Int
compSize3D align a b w h d = (rb + mod (-rb) align) * h * d where rb = compSize1D a b w
