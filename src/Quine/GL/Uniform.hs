{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Quine.GL.Uniform
  ( 
  -- * Uniform Locations
    UniformLocation
  , uniformLocation
  -- * Uniform Access
  , Uniform(..)
  , uniform
  -- * Uniform Types
  , UniformType
  , showUniformType
  , uniformTypeName
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Linear
import Linear.Affine
import Graphics.GL.Core45
import Graphics.GL.Types
import Quine.GL.Program
import Quine.StateVar

-- * Uniform Locations

type UniformLocation = GLint

uniformLocation :: MonadIO m => Program -> String -> m UniformLocation
uniformLocation (Program p) s = liftIO $ withCString s (glGetUniformLocation p . castPtr)

-- * Getting/Setting Uniforms

class Uniform a where
  getUniform :: MonadIO m => Program -> UniformLocation -> m a
  setUniform :: MonadIO m => UniformLocation -> a -> m ()

uniform :: (MonadIO m, Uniform a) => Program -> String -> m (StateVar a)
uniform p s = do
  l <- uniformLocation p s
  return $ StateVar (getUniform p l) (setUniform l)

instance Uniform Float where
  getUniform p l = liftIO $ allocaArray 16 $ (>>) <$> glGetUniformfv (coerce p) l <*> peek
  setUniform = glUniform1f

instance float ~ Float => Uniform (Float,float) where
  getUniform p l = liftIO $ allocaArray 16 $ \ptr -> do
    glGetUniformfv (coerce p) l ptr
    (,) <$> peek ptr <*> peekElemOff ptr 1
  setUniform l = uncurry (glUniform2f l)

instance Uniform (V2 Float) where
  getUniform p l = liftIO $ allocaArray 16 $ \ptr -> do
    glGetUniformfv (coerce p) l (castPtr ptr)
    peek ptr
  setUniform l (V2 a b) = glUniform2f l a b

instance Uniform (V3 Float) where
  getUniform p l = liftIO $ allocaArray 16 $ \ptr -> do
    glGetUniformfv (coerce p) l (castPtr ptr)
    peek ptr
  setUniform l (V3 a b c) = glUniform3f l a b c

instance Uniform (V4 Float) where
  getUniform p l = liftIO $ allocaArray 16 $ \ptr -> do
    glGetUniformfv (coerce p) l (castPtr ptr)
    peek ptr
  setUniform l (V4 a b c d) = glUniform4f l a b c d

instance Uniform (Quaternion Float) where
  getUniform p l = liftIO $ allocaArray 16 $ \ptr -> do
    glGetUniformfv (coerce p) l (castPtr ptr)
    peek ptr
  setUniform l (Quaternion a (V3 b c d)) = glUniform4f l a b c d

instance Uniform (f a) => Uniform (Point f a) where
  getUniform p l = P `liftM` getUniform p l 
  setUniform l (P a) = setUniform l a

-- * Uniform Types

type UniformType = GLenum

showUniformType :: Int -> UniformType -> ShowS
showUniformType d = \case
  GL_FLOAT -> showString "GL_FLOAT"
  GL_FLOAT_VEC2 -> showString "GL_FLOAT_VEC2"
  GL_FLOAT_VEC3 -> showString "GL_FLOAT_VEC3"
  GL_FLOAT_VEC4 -> showString "GL_FLOAT_VEC4"
  GL_DOUBLE -> showString "GL_DOUBLE"
  GL_DOUBLE_VEC2 -> showString "GL_DOUBLE_VEC2"
  GL_DOUBLE_VEC3 -> showString "GL_DOUBLE_VEC3"
  GL_DOUBLE_VEC4 -> showString "GL_DOUBLE_VEC4"
  GL_INT -> showString "GL_INT"
  GL_INT_VEC2 -> showString "GL_INT_VEC2"
  GL_INT_VEC3 -> showString "GL_INT_VEC3"
  GL_INT_VEC4 -> showString "GL_INT_VEC4"
  GL_UNSIGNED_INT -> showString "GL_UNSIGNED_INT"
  GL_UNSIGNED_INT_VEC2 -> showString "GL_UNSIGNED_INT_VEC2"
  GL_UNSIGNED_INT_VEC3 -> showString "GL_UNSIGNED_INT_VEC3"
  GL_UNSIGNED_INT_VEC4 -> showString "GL_UNSIGNED_INT_VEC4"
  GL_BOOL -> showString "GL_BOOL"
  GL_BOOL_VEC2 -> showString "GL_BOOL_VEC2"
  GL_BOOL_VEC3 -> showString "GL_BOOL_VEC3"
  GL_BOOL_VEC4 -> showString "GL_BOOL_VEC4"
  GL_FLOAT_MAT2 -> showString "GL_FLOAT_MAT2"
  GL_FLOAT_MAT3 -> showString "GL_FLOAT_MAT3"
  GL_FLOAT_MAT4 -> showString "GL_FLOAT_MAT4"
  GL_FLOAT_MAT2x3 -> showString "GL_FLOAT_MAT2x3"
  GL_FLOAT_MAT2x4 -> showString "GL_FLOAT_MAT2x4"
  GL_FLOAT_MAT3x2 -> showString "GL_FLOAT_MAT3x2"
  GL_FLOAT_MAT3x4 -> showString "GL_FLOAT_MAT3x4"
  GL_FLOAT_MAT4x2 -> showString "GL_FLOAT_MAT4x2"
  GL_FLOAT_MAT4x3 -> showString "GL_FLOAT_MAT4x3"
  GL_DOUBLE_MAT2 -> showString "GL_DOUBLE_MAT2"
  GL_DOUBLE_MAT3 -> showString "GL_DOUBLE_MAT3"
  GL_DOUBLE_MAT4 -> showString "GL_DOUBLE_MAT4"
  GL_DOUBLE_MAT2x3 -> showString "GL_DOUBLE_MAT2x3"
  GL_DOUBLE_MAT2x4 -> showString "GL_DOUBLE_MAT2x4"
  GL_DOUBLE_MAT3x2 -> showString "GL_DOUBLE_MAT3x2"
  GL_DOUBLE_MAT3x4 -> showString "GL_DOUBLE_MAT3x4"
  GL_DOUBLE_MAT4x2 -> showString "GL_DOUBLE_MAT4x2"
  GL_DOUBLE_MAT4x3 -> showString "GL_DOUBLE_MAT4x3"
  GL_SAMPLER_1D -> showString "GL_SAMPLER_1D"
  GL_SAMPLER_2D -> showString "GL_SAMPLER_2D"
  GL_SAMPLER_3D -> showString "GL_SAMPLER_3D"
  GL_SAMPLER_CUBE -> showString "GL_SAMPLER_CUBE"
  GL_SAMPLER_1D_SHADOW -> showString "GL_SAMPLER_1D_SHADOW"
  GL_SAMPLER_2D_SHADOW -> showString "GL_SAMPLER_2D_SHADOW"
  GL_SAMPLER_1D_ARRAY -> showString "GL_SAMPLER_1D_ARRAY"
  GL_SAMPLER_2D_ARRAY -> showString "GL_SAMPLER_2D_ARRAY"
  GL_SAMPLER_1D_ARRAY_SHADOW -> showString "GL_SAMPLER_1D_ARRAY_SHADOW"
  GL_SAMPLER_2D_ARRAY_SHADOW -> showString "GL_SAMPLER_2D_ARRAY_SHADOW"
  GL_SAMPLER_2D_MULTISAMPLE -> showString "GL_SAMPLER_2D_MULTISAMPLE"
  GL_SAMPLER_2D_MULTISAMPLE_ARRAY -> showString "GL_SAMPLER_2D_MULTISAMPLE_ARRAY"
  GL_SAMPLER_CUBE_SHADOW -> showString "GL_SAMPLER_CUBE_SHADOW"
  GL_SAMPLER_BUFFER -> showString "GL_SAMPLER_BUFFER"
  GL_SAMPLER_2D_RECT -> showString "GL_SAMPLER_2D_RECT"
  GL_SAMPLER_2D_RECT_SHADOW -> showString "GL_SAMPLER_2D_RECT_SHADOW"
  GL_INT_SAMPLER_1D -> showString "GL_INT_SAMPLER_1D"
  GL_INT_SAMPLER_2D -> showString "GL_INT_SAMPLER_2D"
  GL_INT_SAMPLER_3D -> showString "GL_INT_SAMPLER_3D"
  GL_INT_SAMPLER_CUBE -> showString "GL_INT_SAMPLER_CUBE"
  GL_INT_SAMPLER_1D_ARRAY -> showString "GL_INT_SAMPLER_1D_ARRAY"
  GL_INT_SAMPLER_2D_ARRAY -> showString "GL_INT_SAMPLER_2D_ARRAY"
  GL_INT_SAMPLER_2D_MULTISAMPLE -> showString "GL_INT_SAMPLER_2D_MULTISAMPLE"
  GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY -> showString "GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY"
  GL_INT_SAMPLER_BUFFER -> showString "GL_INT_SAMPLER_BUFFER"
  GL_INT_SAMPLER_2D_RECT -> showString "GL_INT_SAMPLER_2D_RECT"
  GL_UNSIGNED_INT_SAMPLER_1D -> showString "GL_UNSIGNED_INT_SAMPLER_1D"
  GL_UNSIGNED_INT_SAMPLER_2D -> showString "GL_UNSIGNED_INT_SAMPLER_2D"
  GL_UNSIGNED_INT_SAMPLER_3D -> showString "GL_UNSIGNED_INT_SAMPLER_3D"
  GL_UNSIGNED_INT_SAMPLER_CUBE -> showString "GL_UNSIGNED_INT_SAMPLER_CUBE"
  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY -> showString "GL_UNSIGNED_INT_SAMPLER_1D_ARRAY"
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY -> showString "GL_UNSIGNED_INT_SAMPLER_2D_ARRAY"
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE -> showString "GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE"
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY -> showString "GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY"
  GL_UNSIGNED_INT_SAMPLER_BUFFER -> showString "GL_UNSIGNED_INT_SAMPLER_BUFFER"
  GL_UNSIGNED_INT_SAMPLER_2D_RECT -> showString "GL_UNSIGNED_INT_SAMPLER_2D_RECT"
  GL_IMAGE_1D -> showString "GL_IMAGE_1D"
  GL_IMAGE_2D -> showString "GL_IMAGE_2D"
  GL_IMAGE_3D -> showString "GL_IMAGE_3D"
  GL_IMAGE_2D_RECT -> showString "GL_IMAGE_2D_RECT"
  GL_IMAGE_CUBE -> showString "GL_IMAGE_CUBE"
  GL_IMAGE_BUFFER -> showString "GL_IMAGE_BUFFER"
  GL_IMAGE_1D_ARRAY -> showString "GL_IMAGE_1D_ARRAY"
  GL_IMAGE_2D_ARRAY -> showString "GL_IMAGE_2D_ARRAY"
  GL_IMAGE_2D_MULTISAMPLE -> showString "GL_IMAGE_2D_MULTISAMPLE"
  GL_IMAGE_2D_MULTISAMPLE_ARRAY -> showString "GL_IMAGE_2D_MULTISAMPLE_ARRAY"
  GL_INT_IMAGE_1D -> showString "GL_INT_IMAGE_1D"
  GL_INT_IMAGE_2D -> showString "GL_INT_IMAGE_2D"
  GL_INT_IMAGE_3D -> showString "GL_INT_IMAGE_3D"
  GL_INT_IMAGE_2D_RECT -> showString "GL_INT_IMAGE_2D_RECT"
  GL_INT_IMAGE_CUBE -> showString "GL_INT_IMAGE_CUBE"
  GL_INT_IMAGE_BUFFER -> showString "GL_INT_IMAGE_BUFFER"
  GL_INT_IMAGE_1D_ARRAY -> showString "GL_INT_IMAGE_1D_ARRAY"
  GL_INT_IMAGE_2D_ARRAY -> showString "GL_INT_IMAGE_2D_ARRAY"
  GL_INT_IMAGE_2D_MULTISAMPLE -> showString "GL_INT_IMAGE_2D_MULTISAMPLE"
  GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY -> showString "GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY"
  GL_UNSIGNED_INT_IMAGE_1D -> showString "GL_UNSIGNED_INT_IMAGE_1D"
  GL_UNSIGNED_INT_IMAGE_2D -> showString "GL_UNSIGNED_INT_IMAGE_2D"
  GL_UNSIGNED_INT_IMAGE_3D -> showString "GL_UNSIGNED_INT_IMAGE_3D"
  GL_UNSIGNED_INT_IMAGE_2D_RECT -> showString "GL_UNSIGNED_INT_IMAGE_2D_RECT"
  GL_UNSIGNED_INT_IMAGE_CUBE -> showString "GL_UNSIGNED_INT_IMAGE_CUBE"
  GL_UNSIGNED_INT_IMAGE_BUFFER -> showString "GL_UNSIGNED_INT_IMAGE_BUFFER"
  GL_UNSIGNED_INT_IMAGE_1D_ARRAY -> showString "GL_UNSIGNED_INT_IMAGE_1D_ARRAY"
  GL_UNSIGNED_INT_IMAGE_2D_ARRAY -> showString "GL_UNSIGNED_INT_IMAGE_2D_ARRAY"
  GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE -> showString "GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE"
  GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY -> showString "GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY"
  GL_UNSIGNED_INT_ATOMIC_COUNTER -> showString "GL_UNSIGNED_INT_ATOMIC_COUNTER"
  t -> showsPrec d t

uniformTypeName :: UniformType -> Maybe String
uniformTypeName = \ case
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
