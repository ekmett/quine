{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, PatternSynonyms #-}
module Quine.GL.Uniform
  ( 
  -- * Uniform Locations
    UniformLocation(..)
  , uniformLocation
  -- * Uniform Access
  , uniform1f
  , uniform2f
  , uniform3f
  , uniform4f
  -- * Uniform Types
  , UniformType(..)
  , showUniformType
  -- ** Patterns
  , pattern UniformFloat
  , pattern UniformVec2
  , pattern UniformVec3
  , pattern UniformVec4
  , pattern UniformInt
  , pattern UniformIvec2
  , pattern UniformIvec3
  , pattern UniformIvec4
  , pattern UniformUnsignedInt
  , pattern UniformUvec2
  , pattern UniformUvec3
  , pattern UniformUvec4
  , pattern UniformBool
  , pattern UniformBvec2
  , pattern UniformBvec3
  , pattern UniformBvec4
  , pattern UniformMat2
  , pattern UniformMat3
  , pattern UniformMat4
  , pattern UniformMat2x3
  , pattern UniformMat2x4
  , pattern UniformMat3x2
  , pattern UniformMat3x4
  , pattern UniformMat4x2
  , pattern UniformMat4x3
  , pattern UniformSampler1D
  , pattern UniformSampler2D
  , pattern UniformSampler3D
  , pattern UniformSamplerCube
  , pattern UniformSampler1DShadow
  , pattern UniformSampler2DShadow
  , pattern UniformSampler1DArray
  , pattern UniformSampler2DArray
  , pattern UniformSampler1DArrayShadow
  , pattern UniformSampler2DArrayShadow
  , pattern UniformSampler2DMS
  , pattern UniformSampler2DMSArray
  , pattern UniformSamplerCubeShadow
  , pattern UniformSamplerBuffer
  , pattern UniformSampler2DRect
  , pattern UniformSampler2DRectShadow
  , pattern UniformIsampler1D
  , pattern UniformIsampler2D
  , pattern UniformIsampler3D
  , pattern UniformIsamplerCube
  , pattern UniformIsampler1DArray
  , pattern UniformIsampler2DArray
  , pattern UniformIsampler2DMS
  , pattern UniformIsampler2DMSArray
  , pattern UniformIsamplerBuffer
  , pattern UniformIsampler2DRect
  , pattern UniformUsampler1D
  , pattern UniformUsampler2D
  , pattern UniformUsampler3D
  , pattern UniformUsamplerCube
  , pattern UniformUsampler1DArray
  , pattern UniformUsampler2DArray
  , pattern UniformUsampler2DMS
  , pattern UniformUsampler2DMSArray
  , pattern UniformUsamplerBuffer
  , pattern UniformUsampler2DRect
  -- *** GL 4.1+
  , pattern UniformDouble
  , pattern UniformDvec2
  , pattern UniformDvec3
  , pattern UniformDvec4
  , pattern UniformDmat2
  , pattern UniformDmat3
  , pattern UniformDmat4
  , pattern UniformDmat2x3
  , pattern UniformDmat2x4
  , pattern UniformDmat3x2
  , pattern UniformDmat3x4
  , pattern UniformDmat4x2
  , pattern UniformDmat4x3
  -- *** GL 4.2+
  , pattern UniformImage1D
  , pattern UniformImage2D
  , pattern UniformImage3D
  , pattern UniformImage2DRect
  , pattern UniformImageCube
  , pattern UniformImageBuffer
  , pattern UniformImage1DArray
  , pattern UniformImage2DArray
  , pattern UniformImage2DMS
  , pattern UniformImage2DMSArray
  , pattern UniformIimage1D
  , pattern UniformIimage2D
  , pattern UniformIimage3D
  , pattern UniformIimage2DRect
  , pattern UniformIimageCube
  , pattern UniformIimageBuffer
  , pattern UniformIimage1DArray
  , pattern UniformIimage2DArray
  , pattern UniformIimage2DMS
  , pattern UniformIimage2DMSArray
  , pattern UniformUimage1D
  , pattern UniformUimage2D
  , pattern UniformUimage3D
  , pattern UniformUimage2DRect
  , pattern UniformUimageCube
  , pattern UniformUimageBuffer
  , pattern UniformUimage1DArray
  , pattern UniformUimage2DArray
  , pattern UniformUimage2DMS
  , pattern UniformUimage2DMSArray
  , pattern UniformAtomicUnsignedInt
  ) where

import Control.Monad.IO.Class
import Data.Coerce
import Data.Data
import Data.Functor
import Foreign.C.String
import Foreign.Ptr
import GHC.Generics
import Graphics.GL.Raw.Profile.Core45
import Graphics.GL.Raw.Types
import Quine.GL.Program
import Quine.StateVar

-- * Uniform Locations

newtype UniformLocation = UniformLocation GLint 
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

uniformLocation :: MonadIO m => Program -> String -> m UniformLocation
uniformLocation (Program p) s = liftIO $ coerce <$> withCString s (glGetUniformLocation p . castPtr)

-- * Setting Uniforms

uniform1f :: MonadIO m => Program -> String -> m (SettableStateVar Float)
uniform1f p u = do
  l <- uniformLocation p u
  return $ SettableStateVar $ glUniform1f (coerce l)

uniform2f :: MonadIO m => Program -> String -> m (SettableStateVar (Float, Float))
uniform2f p u = do
  l <- uniformLocation p u
  return $ SettableStateVar $ \(a,b) -> glUniform2f (coerce l) a b

uniform3f :: MonadIO m => Program -> String -> m (SettableStateVar (Float, Float, Float))
uniform3f p u = do
  l <- uniformLocation p u
  return $ SettableStateVar $ \(a,b,c) -> glUniform3f (coerce l) a b c

uniform4f :: MonadIO m => Program -> String -> m (SettableStateVar (Float, Float, Float, Float))
uniform4f p u = do
  l <- uniformLocation p u
  return $ SettableStateVar $ \(a,b,c,d) -> glUniform4f (coerce l) a b c d

-- * Uniform Types

newtype UniformType = UniformType GLenum deriving (Eq,Ord,Data,Typeable,Generic)

pattern UniformFloat = UniformType GL_FLOAT
pattern UniformVec2 = UniformType GL_FLOAT_VEC2
pattern UniformVec3 = UniformType GL_FLOAT_VEC3
pattern UniformVec4 = UniformType GL_FLOAT_VEC4
pattern UniformDouble = UniformType GL_DOUBLE
pattern UniformDvec2 = UniformType GL_DOUBLE_VEC2
pattern UniformDvec3 = UniformType GL_DOUBLE_VEC3
pattern UniformDvec4 = UniformType GL_DOUBLE_VEC4
pattern UniformInt = UniformType GL_INT
pattern UniformIvec2 = UniformType GL_INT_VEC2
pattern UniformIvec3 = UniformType GL_INT_VEC3
pattern UniformIvec4 = UniformType GL_INT_VEC4
pattern UniformUnsignedInt = UniformType GL_UNSIGNED_INT
pattern UniformUvec2 = UniformType GL_UNSIGNED_INT_VEC2
pattern UniformUvec3 = UniformType GL_UNSIGNED_INT_VEC3
pattern UniformUvec4 = UniformType GL_UNSIGNED_INT_VEC4
pattern UniformBool = UniformType GL_BOOL
pattern UniformBvec2 = UniformType GL_BOOL_VEC2
pattern UniformBvec3 = UniformType GL_BOOL_VEC3
pattern UniformBvec4 = UniformType GL_BOOL_VEC4
pattern UniformMat2 = UniformType GL_FLOAT_MAT2
pattern UniformMat3 = UniformType GL_FLOAT_MAT3
pattern UniformMat4 = UniformType GL_FLOAT_MAT4
pattern UniformMat2x3 = UniformType GL_FLOAT_MAT2x3
pattern UniformMat2x4 = UniformType GL_FLOAT_MAT2x4
pattern UniformMat3x2 = UniformType GL_FLOAT_MAT3x2
pattern UniformMat3x4 = UniformType GL_FLOAT_MAT3x4
pattern UniformMat4x2 = UniformType GL_FLOAT_MAT4x2
pattern UniformMat4x3 = UniformType GL_FLOAT_MAT4x3
pattern UniformDmat2 = UniformType GL_DOUBLE_MAT2
pattern UniformDmat3 = UniformType GL_DOUBLE_MAT3
pattern UniformDmat4 = UniformType GL_DOUBLE_MAT4
pattern UniformDmat2x3 = UniformType GL_DOUBLE_MAT2x3
pattern UniformDmat2x4 = UniformType GL_DOUBLE_MAT2x4
pattern UniformDmat3x2 = UniformType GL_DOUBLE_MAT3x2
pattern UniformDmat3x4 = UniformType GL_DOUBLE_MAT3x4
pattern UniformDmat4x2 = UniformType GL_DOUBLE_MAT4x2
pattern UniformDmat4x3 = UniformType GL_DOUBLE_MAT4x3
pattern UniformSampler1D = UniformType GL_SAMPLER_1D
pattern UniformSampler2D = UniformType GL_SAMPLER_2D
pattern UniformSampler3D = UniformType GL_SAMPLER_3D
pattern UniformSamplerCube = UniformType GL_SAMPLER_CUBE
pattern UniformSampler1DShadow = UniformType GL_SAMPLER_1D_SHADOW
pattern UniformSampler2DShadow = UniformType GL_SAMPLER_2D_SHADOW
pattern UniformSampler1DArray = UniformType GL_SAMPLER_1D_ARRAY
pattern UniformSampler2DArray = UniformType GL_SAMPLER_2D_ARRAY
pattern UniformSampler1DArrayShadow = UniformType GL_SAMPLER_1D_ARRAY_SHADOW
pattern UniformSampler2DArrayShadow = UniformType GL_SAMPLER_2D_ARRAY_SHADOW
pattern UniformSampler2DMS = UniformType GL_SAMPLER_2D_MULTISAMPLE
pattern UniformSampler2DMSArray = UniformType GL_SAMPLER_2D_MULTISAMPLE_ARRAY
pattern UniformSamplerCubeShadow = UniformType GL_SAMPLER_CUBE_SHADOW
pattern UniformSamplerBuffer = UniformType GL_SAMPLER_BUFFER
pattern UniformSampler2DRect = UniformType GL_SAMPLER_2D_RECT
pattern UniformSampler2DRectShadow = UniformType GL_SAMPLER_2D_RECT_SHADOW
pattern UniformIsampler1D = UniformType GL_INT_SAMPLER_1D
pattern UniformIsampler2D = UniformType GL_INT_SAMPLER_2D
pattern UniformIsampler3D = UniformType GL_INT_SAMPLER_3D
pattern UniformIsamplerCube = UniformType GL_INT_SAMPLER_CUBE
pattern UniformIsampler1DArray = UniformType GL_INT_SAMPLER_1D_ARRAY
pattern UniformIsampler2DArray = UniformType GL_INT_SAMPLER_2D_ARRAY
pattern UniformIsampler2DMS = UniformType GL_INT_SAMPLER_2D_MULTISAMPLE
pattern UniformIsampler2DMSArray = UniformType GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
pattern UniformIsamplerBuffer = UniformType GL_INT_SAMPLER_BUFFER
pattern UniformIsampler2DRect = UniformType GL_INT_SAMPLER_2D_RECT
pattern UniformUsampler1D = UniformType GL_UNSIGNED_INT_SAMPLER_1D
pattern UniformUsampler2D = UniformType GL_UNSIGNED_INT_SAMPLER_2D
pattern UniformUsampler3D = UniformType GL_UNSIGNED_INT_SAMPLER_3D
pattern UniformUsamplerCube = UniformType GL_UNSIGNED_INT_SAMPLER_CUBE
pattern UniformUsampler1DArray = UniformType GL_UNSIGNED_INT_SAMPLER_1D_ARRAY
pattern UniformUsampler2DArray = UniformType GL_UNSIGNED_INT_SAMPLER_2D_ARRAY
pattern UniformUsampler2DMS = UniformType GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE
pattern UniformUsampler2DMSArray = UniformType GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
pattern UniformUsamplerBuffer = UniformType GL_UNSIGNED_INT_SAMPLER_BUFFER
pattern UniformUsampler2DRect = UniformType GL_UNSIGNED_INT_SAMPLER_2D_RECT
pattern UniformImage1D = UniformType GL_IMAGE_1D
pattern UniformImage2D = UniformType GL_IMAGE_2D
pattern UniformImage3D = UniformType GL_IMAGE_3D
pattern UniformImage2DRect = UniformType GL_IMAGE_2D_RECT
pattern UniformImageCube = UniformType GL_IMAGE_CUBE
pattern UniformImageBuffer = UniformType GL_IMAGE_BUFFER
pattern UniformImage1DArray = UniformType GL_IMAGE_1D_ARRAY
pattern UniformImage2DArray = UniformType GL_IMAGE_2D_ARRAY
pattern UniformImage2DMS = UniformType GL_IMAGE_2D_MULTISAMPLE
pattern UniformImage2DMSArray = UniformType GL_IMAGE_2D_MULTISAMPLE_ARRAY
pattern UniformIimage1D = UniformType GL_INT_IMAGE_1D
pattern UniformIimage2D = UniformType GL_INT_IMAGE_2D
pattern UniformIimage3D = UniformType GL_INT_IMAGE_3D
pattern UniformIimage2DRect = UniformType GL_INT_IMAGE_2D_RECT
pattern UniformIimageCube = UniformType GL_INT_IMAGE_CUBE
pattern UniformIimageBuffer = UniformType GL_INT_IMAGE_BUFFER
pattern UniformIimage1DArray = UniformType GL_INT_IMAGE_1D_ARRAY
pattern UniformIimage2DArray = UniformType GL_INT_IMAGE_2D_ARRAY
pattern UniformIimage2DMS = UniformType GL_INT_IMAGE_2D_MULTISAMPLE
pattern UniformIimage2DMSArray = UniformType GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY
pattern UniformUimage1D = UniformType GL_UNSIGNED_INT_IMAGE_1D
pattern UniformUimage2D = UniformType GL_UNSIGNED_INT_IMAGE_2D
pattern UniformUimage3D = UniformType GL_UNSIGNED_INT_IMAGE_3D
pattern UniformUimage2DRect = UniformType GL_UNSIGNED_INT_IMAGE_2D_RECT
pattern UniformUimageCube = UniformType GL_UNSIGNED_INT_IMAGE_CUBE
pattern UniformUimageBuffer = UniformType GL_UNSIGNED_INT_IMAGE_BUFFER
pattern UniformUimage1DArray = UniformType GL_UNSIGNED_INT_IMAGE_1D_ARRAY
pattern UniformUimage2DArray = UniformType GL_UNSIGNED_INT_IMAGE_2D_ARRAY
pattern UniformUimage2DMS = UniformType GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE
pattern UniformUimage2DMSArray = UniformType GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY
pattern UniformAtomicUnsignedInt = UniformType GL_UNSIGNED_INT_ATOMIC_COUNTER

instance Show UniformType where
  showsPrec d (UniformType t) = case t of
    GL_FLOAT -> showString "UniformFloat"
    GL_FLOAT_VEC2 -> showString "UniformVec2"
    GL_FLOAT_VEC3 -> showString "UniformVec3"
    GL_FLOAT_VEC4 -> showString "UniformVec4"
    GL_DOUBLE -> showString "UniformDouble"
    GL_DOUBLE_VEC2 -> showString "UniformDvec2"
    GL_DOUBLE_VEC3 -> showString "UniformDvec3"
    GL_DOUBLE_VEC4 -> showString "UniformDvec4"
    GL_INT -> showString "UniformInt"
    GL_INT_VEC2 -> showString "UniformIvec2"
    GL_INT_VEC3 -> showString "UniformIvec3"
    GL_INT_VEC4 -> showString "UniformIvec4"
    GL_UNSIGNED_INT -> showString "UniformUnsignedInt"
    GL_UNSIGNED_INT_VEC2 -> showString "UniformUvec2"
    GL_UNSIGNED_INT_VEC3 -> showString "UniformUvec3"
    GL_UNSIGNED_INT_VEC4 -> showString "UniformUvec4"
    GL_BOOL -> showString "UniformBool"
    GL_BOOL_VEC2 -> showString "UniformBvec2"
    GL_BOOL_VEC3 -> showString "UniformBvec3"
    GL_BOOL_VEC4 -> showString "UniformBvec4"
    GL_FLOAT_MAT2 -> showString "UniformMat2"
    GL_FLOAT_MAT3 -> showString "UniformMat3"
    GL_FLOAT_MAT4 -> showString "UniformMat4"
    GL_FLOAT_MAT2x3 -> showString "UniformMat2x3"
    GL_FLOAT_MAT2x4 -> showString "UniformMat2x4"
    GL_FLOAT_MAT3x2 -> showString "UniformMat3x2"
    GL_FLOAT_MAT3x4 -> showString "UniformMat3x4"
    GL_FLOAT_MAT4x2 -> showString "UniformMat4x2"
    GL_FLOAT_MAT4x3 -> showString "UniformMat4x3"
    GL_DOUBLE_MAT2 -> showString "UniformDmat2"
    GL_DOUBLE_MAT3 -> showString "UniformDmat3"
    GL_DOUBLE_MAT4 -> showString "UniformDmat4"
    GL_DOUBLE_MAT2x3 -> showString "UniformDmat2x3"
    GL_DOUBLE_MAT2x4 -> showString "UniformDmat2x4"
    GL_DOUBLE_MAT3x2 -> showString "UniformDmat3x2"
    GL_DOUBLE_MAT3x4 -> showString "UniformDmat3x4"
    GL_DOUBLE_MAT4x2 -> showString "UniformDmat4x2"
    GL_DOUBLE_MAT4x3 -> showString "UniformDmat4x3"
    GL_SAMPLER_1D -> showString "UniformSampler1D"
    GL_SAMPLER_2D -> showString "UniformSampler2D"
    GL_SAMPLER_3D -> showString "UniformSampler3D"
    GL_SAMPLER_CUBE -> showString "UniformSamplerCube"
    GL_SAMPLER_1D_SHADOW -> showString "UniformSampler1DShadow"
    GL_SAMPLER_2D_SHADOW -> showString "UniformSampler2DShadow"
    GL_SAMPLER_1D_ARRAY -> showString "UniformSampler1DArray"
    GL_SAMPLER_2D_ARRAY -> showString "UniformSampler2DArray"
    GL_SAMPLER_1D_ARRAY_SHADOW -> showString "UniformSampler1DArrayShadow"
    GL_SAMPLER_2D_ARRAY_SHADOW -> showString "UniformSampler2DArrayShadow"
    GL_SAMPLER_2D_MULTISAMPLE -> showString "UniformSampler2DMS"
    GL_SAMPLER_2D_MULTISAMPLE_ARRAY -> showString "UniformSampler2DMSArray"
    GL_SAMPLER_CUBE_SHADOW -> showString "UniformSamplerCubeShadow"
    GL_SAMPLER_BUFFER -> showString "UniformSamplerBuffer"
    GL_SAMPLER_2D_RECT -> showString "UniformSampler2DRect"
    GL_SAMPLER_2D_RECT_SHADOW -> showString "UniformSampler2DRectShadow"
    GL_INT_SAMPLER_1D -> showString "UniformIsampler1D"
    GL_INT_SAMPLER_2D -> showString "UniformIsampler2D"
    GL_INT_SAMPLER_3D -> showString "UniformIsampler3D"
    GL_INT_SAMPLER_CUBE -> showString "UniformIsamplerCube"
    GL_INT_SAMPLER_1D_ARRAY -> showString "UniformIsampler1DArray"
    GL_INT_SAMPLER_2D_ARRAY -> showString "UniformIsampler2DArray"
    GL_INT_SAMPLER_2D_MULTISAMPLE -> showString "UniformIsampler2DMS"
    GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY -> showString "UniformIsampler2DMSArray"
    GL_INT_SAMPLER_BUFFER -> showString "UniformIsamplerBuffer"
    GL_INT_SAMPLER_2D_RECT -> showString "UniformIsampler2DRect"
    GL_UNSIGNED_INT_SAMPLER_1D -> showString "UniformUsampler1D"
    GL_UNSIGNED_INT_SAMPLER_2D -> showString "UniformUsampler2D"
    GL_UNSIGNED_INT_SAMPLER_3D -> showString "UniformUsampler3D"
    GL_UNSIGNED_INT_SAMPLER_CUBE -> showString "UniformUsamplerCube"
    GL_UNSIGNED_INT_SAMPLER_1D_ARRAY -> showString "UniformUsampler1DArray"
    GL_UNSIGNED_INT_SAMPLER_2D_ARRAY -> showString "UniformUsampler2DArray"
    GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE -> showString "UniformUsampler2DMS"
    GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY -> showString "UniformUsampler2DMSArray"
    GL_UNSIGNED_INT_SAMPLER_BUFFER -> showString "UniformUsamplerBuffer"
    GL_UNSIGNED_INT_SAMPLER_2D_RECT -> showString "UniformUsampler2DRect"
    GL_IMAGE_1D -> showString "UniformImage1D"
    GL_IMAGE_2D -> showString "UniformImage2D"
    GL_IMAGE_3D -> showString "UniformImage3D"
    GL_IMAGE_2D_RECT -> showString "UniformImage2DRect"
    GL_IMAGE_CUBE -> showString "UniformImageCube"
    GL_IMAGE_BUFFER -> showString "UniformImageBuffer"
    GL_IMAGE_1D_ARRAY -> showString "UniformImage1DArray"
    GL_IMAGE_2D_ARRAY -> showString "UniformImage2DArray"
    GL_IMAGE_2D_MULTISAMPLE -> showString "UniformImage2DMS"
    GL_IMAGE_2D_MULTISAMPLE_ARRAY -> showString "UniformImage2DMSArray"
    GL_INT_IMAGE_1D -> showString "UniformIimage1D"
    GL_INT_IMAGE_2D -> showString "UniformIimage2D"
    GL_INT_IMAGE_3D -> showString "UniformIimage3D"
    GL_INT_IMAGE_2D_RECT -> showString "UniformIimage2DRect"
    GL_INT_IMAGE_CUBE -> showString "UniformIimageCube"
    GL_INT_IMAGE_BUFFER -> showString "UniformIimageBuffer"
    GL_INT_IMAGE_1D_ARRAY -> showString "UniformIimage1DArray"
    GL_INT_IMAGE_2D_ARRAY -> showString "UniformIimage2DArray"
    GL_INT_IMAGE_2D_MULTISAMPLE -> showString "UniformIimage2DMS"
    GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY -> showString "UniformIimage2DMSArray"
    GL_UNSIGNED_INT_IMAGE_1D -> showString "UniformUimage1D"
    GL_UNSIGNED_INT_IMAGE_2D -> showString "UniformUimage2D"
    GL_UNSIGNED_INT_IMAGE_3D -> showString "UniformUimage3D"
    GL_UNSIGNED_INT_IMAGE_2D_RECT -> showString "UniformUimage2DRect"
    GL_UNSIGNED_INT_IMAGE_CUBE -> showString "UniformUimageCube"
    GL_UNSIGNED_INT_IMAGE_BUFFER -> showString "UniformUimageBuffer"
    GL_UNSIGNED_INT_IMAGE_1D_ARRAY -> showString "UniformUimage1DArray"
    GL_UNSIGNED_INT_IMAGE_2D_ARRAY -> showString "UniformUimage2DArray"
    GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE -> showString "UniformUimage2DMS"
    GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY -> showString "UniformUimage2DMSArray"
    GL_UNSIGNED_INT_ATOMIC_COUNTER -> showString "UniformAtomicUnsignedInt"
    other -> showParen (d > 10) $ showString "UniformType " . showsPrec 11 other

showUniformType :: UniformType -> Maybe String
showUniformType (UniformType e) = case e of
  GL_FLOAT -> Just "float"
  GL_FLOAT_VEC2 -> Just "vec2"
  GL_FLOAT_VEC3 -> Just "vec3"
  GL_FLOAT_VEC4 -> Just "vec4"
  GL_DOUBLE -> Just "double"
  GL_DOUBLE_VEC2 -> Just "dvec2"
  GL_DOUBLE_VEC3 -> Just "dvec3"
  GL_DOUBLE_VEC4 -> Just "dvec4"
  GL_INT -> Just "int"
  GL_INT_VEC2 -> Just "ivec2"
  GL_INT_VEC3 -> Just "ivec3"
  GL_INT_VEC4 -> Just "ivec4"
  GL_UNSIGNED_INT -> Just "unsigned int"
  GL_UNSIGNED_INT_VEC2 -> Just "uvec2"
  GL_UNSIGNED_INT_VEC3 -> Just "uvec3"
  GL_UNSIGNED_INT_VEC4 -> Just "uvec4"
  GL_BOOL -> Just "bool"
  GL_BOOL_VEC2 -> Just "bvec2"
  GL_BOOL_VEC3 -> Just "bvec3"
  GL_BOOL_VEC4 -> Just "bvec4"
  GL_FLOAT_MAT2 -> Just "mat2"
  GL_FLOAT_MAT3 -> Just "mat3"
  GL_FLOAT_MAT4 -> Just "mat4"
  GL_FLOAT_MAT2x3 -> Just "mat2x3"
  GL_FLOAT_MAT2x4 -> Just "mat2x4"
  GL_FLOAT_MAT3x2 -> Just "mat3x2"
  GL_FLOAT_MAT3x4 -> Just "mat3x4"
  GL_FLOAT_MAT4x2 -> Just "mat4x2"
  GL_FLOAT_MAT4x3 -> Just "mat4x3"
  GL_DOUBLE_MAT2 -> Just "dmat2"
  GL_DOUBLE_MAT3 -> Just "dmat3"
  GL_DOUBLE_MAT4 -> Just "dmat4"
  GL_DOUBLE_MAT2x3 -> Just "dmat2x3"
  GL_DOUBLE_MAT2x4 -> Just "dmat2x4"
  GL_DOUBLE_MAT3x2 -> Just "dmat3x2"
  GL_DOUBLE_MAT3x4 -> Just "dmat3x4"
  GL_DOUBLE_MAT4x2 -> Just "dmat4x2"
  GL_DOUBLE_MAT4x3 -> Just "dmat4x3"
  GL_SAMPLER_1D -> Just "sampler1D"
  GL_SAMPLER_2D -> Just "sampler2D"
  GL_SAMPLER_3D -> Just "sampler3D"
  GL_SAMPLER_CUBE -> Just "samplerCube"
  GL_SAMPLER_1D_SHADOW -> Just "sampler1DShadow"
  GL_SAMPLER_2D_SHADOW -> Just "sampler2DShadow"
  GL_SAMPLER_1D_ARRAY -> Just "sampler1DArray"
  GL_SAMPLER_2D_ARRAY -> Just "sampler2DArray"
  GL_SAMPLER_1D_ARRAY_SHADOW -> Just "sampler1DArrayShadow"
  GL_SAMPLER_2D_ARRAY_SHADOW -> Just "sampler2DArrayShadow"
  GL_SAMPLER_2D_MULTISAMPLE -> Just "sampler2DMS"
  GL_SAMPLER_2D_MULTISAMPLE_ARRAY -> Just "sampler2DMSArray"
  GL_SAMPLER_CUBE_SHADOW -> Just "samplerCubeShadow"
  GL_SAMPLER_BUFFER -> Just "samplerBuffer"
  GL_SAMPLER_2D_RECT -> Just "sampler2DRect"
  GL_SAMPLER_2D_RECT_SHADOW -> Just "sampler2DRectShadow"
  GL_INT_SAMPLER_1D -> Just "isampler1D"
  GL_INT_SAMPLER_2D -> Just "isampler2D"
  GL_INT_SAMPLER_3D -> Just "isampler3D"
  GL_INT_SAMPLER_CUBE -> Just "isamplerCube"
  GL_INT_SAMPLER_1D_ARRAY -> Just "isampler1DArray"
  GL_INT_SAMPLER_2D_ARRAY -> Just "isampler2DArray"
  GL_INT_SAMPLER_2D_MULTISAMPLE -> Just "isampler2DMS"
  GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY -> Just "isampler2DMSArray"
  GL_INT_SAMPLER_BUFFER -> Just "isamplerBuffer"
  GL_INT_SAMPLER_2D_RECT -> Just "isampler2DRect"
  GL_UNSIGNED_INT_SAMPLER_1D -> Just "usampler1D"
  GL_UNSIGNED_INT_SAMPLER_2D -> Just "usampler2D"
  GL_UNSIGNED_INT_SAMPLER_3D -> Just "usampler3D"
  GL_UNSIGNED_INT_SAMPLER_CUBE -> Just "usamplerCube"
  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY -> Just "usampler1DArray"
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY -> Just "usampler2DArray"
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE -> Just "usampler2DMS"
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY -> Just "usampler2DMSArray"
  GL_UNSIGNED_INT_SAMPLER_BUFFER -> Just "usamplerBuffer"
  GL_UNSIGNED_INT_SAMPLER_2D_RECT -> Just "usampler2DRect"
  GL_IMAGE_1D -> Just "image1D"
  GL_IMAGE_2D -> Just "image2D"
  GL_IMAGE_3D -> Just "image3D"
  GL_IMAGE_2D_RECT -> Just "image2DRect"
  GL_IMAGE_CUBE -> Just "imageCube"
  GL_IMAGE_BUFFER -> Just "imageBuffer"
  GL_IMAGE_1D_ARRAY -> Just "image1DArray"
  GL_IMAGE_2D_ARRAY -> Just "image2DArray"
  GL_IMAGE_2D_MULTISAMPLE -> Just "image2DMS"
  GL_IMAGE_2D_MULTISAMPLE_ARRAY -> Just "image2DMSArray"
  GL_INT_IMAGE_1D -> Just "iimage1D"
  GL_INT_IMAGE_2D -> Just "iimage2D"
  GL_INT_IMAGE_3D -> Just "iimage3D"
  GL_INT_IMAGE_2D_RECT -> Just "iimage2DRect"
  GL_INT_IMAGE_CUBE -> Just "iimageCube"
  GL_INT_IMAGE_BUFFER -> Just "iimageBuffer"
  GL_INT_IMAGE_1D_ARRAY -> Just "iimage1DArray"
  GL_INT_IMAGE_2D_ARRAY -> Just "iimage2DArray"
  GL_INT_IMAGE_2D_MULTISAMPLE -> Just "iimage2DMS"
  GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY -> Just "iimage2DMSArray"
  GL_UNSIGNED_INT_IMAGE_1D -> Just "uimage1D"
  GL_UNSIGNED_INT_IMAGE_2D -> Just "uimage2D"
  GL_UNSIGNED_INT_IMAGE_3D -> Just "uimage3D"
  GL_UNSIGNED_INT_IMAGE_2D_RECT -> Just "uimage2DRect"
  GL_UNSIGNED_INT_IMAGE_CUBE -> Just "uimageCube"
  GL_UNSIGNED_INT_IMAGE_BUFFER -> Just "uimageBuffer"
  GL_UNSIGNED_INT_IMAGE_1D_ARRAY -> Just "uimage1DArray"
  GL_UNSIGNED_INT_IMAGE_2D_ARRAY -> Just "uimage2DArray"
  GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE -> Just "uimage2DMS"
  GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY -> Just "uimage2DMSArray"
  GL_UNSIGNED_INT_ATOMIC_COUNTER -> Just "atomic_uint"
  _ -> Nothing
