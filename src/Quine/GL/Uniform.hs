{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
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
--------------------------------------------------------------------
module Quine.GL.Uniform
  (
  -- * Uniform Locations
    UniformLocation
  , uniformLocation
  , uniformMat4s
  , uniformMat4
  -- * Program Uniforms
  -- $programUniform
  , programUniform1f
  , programUniform2f
  , programUniform3f
  , programUniform4f
  , programUniform1d
  , programUniform2d
  , programUniform3d
  , programUniform4d
  , programUniform1i
  , programUniform2i
  , programUniform3i
  , programUniform4i
  , programUniform1ui
  , programUniform2ui
  , programUniform3ui
  , programUniform4ui
  -- * Uniform Types
  , UniformType
  , showUniformType
  , uniformTypeName
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Coerce
import Data.Distributive
import Data.Foldable
import Data.Int
import Data.Version
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear
import Quine.GL.Program
import Quine.GL.Types
import Quine.GL.Version
import Quine.StateVar

--------------------------------------------------------------------------------
-- * Uniform Locations
--------------------------------------------------------------------------------

type UniformLocation = GLint

uniformLocation :: MonadIO m => Program -> String -> m UniformLocation
uniformLocation (Program p) s = liftIO $ withCString s (glGetUniformLocation p . castPtr)

canTranspose :: Bool
canTranspose = not (gles && version < Version [3,1] []) -- older opengl es doesn't support transpose in glUniformMatrix

uniformMatrices
  :: (MonadIO m, Foldable f, Storable (g (h a)), Storable (h (g a)), Distributive h, Functor g)
  => (GLint -> GLsizei -> GLboolean -> Ptr x -> IO ())
  -> (GLint -> GLsizei -> GLboolean -> Ptr y -> IO ())
  -> UniformLocation -> f (g (h a)) -> m ()
uniformMatrices rowMajor columnMajor loc xs
  | canTranspose = liftIO $ withArrayLen (              toList xs) $ \n p -> rowMajor    loc (fromIntegral n) GL_TRUE  (castPtr p)
  | otherwise    = liftIO $ withArrayLen (transpose <$> toList xs) $ \n p -> columnMajor loc (fromIntegral n) GL_FALSE (castPtr p)

data Id a = Id a deriving Foldable

uniformMat4s :: (MonadIO m, Foldable f)  => UniformLocation -> f Mat4 -> m ()
uniformMat4s = uniformMatrices glUniformMatrix4fv glUniformMatrix4fv

uniformMat4 :: MonadIO m  => UniformLocation -> Mat4 -> m ()
uniformMat4 l = uniformMat4s l . Id

--------------------------------------------------------------------------------
-- * Program Uniforms
--------------------------------------------------------------------------------

-- $programUniform
--
-- Requires 'gl_ARB_separate_shader_objects' or OpenGL 4.1+
--
-- The benefit of this API is that it doesn't tie to the current bound program.
-- and so we have fewer state changes, and nicely these can make a full
-- 'StateVar'.

programUniform1f :: Program -> UniformLocation -> StateVar Float
programUniform1f p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformfv (coerce p) (coerce l) . castPtr <*> peek
  s = glProgramUniform1f (coerce p) (coerce l)

programUniform2f :: Program -> UniformLocation -> StateVar (V2 Float)
programUniform2f p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformfv (coerce p) (coerce l) . castPtr <*> peek
  s (V2 a b) = glProgramUniform2f (coerce p) (coerce l) a b

programUniform3f :: Program -> UniformLocation -> StateVar (V3 Float)
programUniform3f p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformfv (coerce p) (coerce l) . castPtr <*> peek
  s (V3 a b c) = glProgramUniform3f (coerce p) (coerce l) a b c

programUniform4f :: Program -> UniformLocation -> StateVar (V4 Float)
programUniform4f p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformfv (coerce p) (coerce l) . castPtr <*> peek
  s (V4 a b c d) = glProgramUniform4f (coerce p) (coerce l) a b c d

programUniform1d :: Program -> UniformLocation -> StateVar Double
programUniform1d p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformdv (coerce p) (coerce l) . castPtr <*> peek
  s = glProgramUniform1d (coerce p) (coerce l)

programUniform2d :: Program -> UniformLocation -> StateVar (V2 Double)
programUniform2d p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformdv (coerce p) (coerce l) . castPtr <*> peek
  s (V2 a b) = glProgramUniform2d (coerce p) (coerce l) a b

programUniform3d :: Program -> UniformLocation -> StateVar (V3 Double)
programUniform3d p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformdv (coerce p) (coerce l) . castPtr <*> peek
  s (V3 a b c) = glProgramUniform3d (coerce p) (coerce l) a b c

programUniform4d :: Program -> UniformLocation -> StateVar (V4 Double)
programUniform4d p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformdv (coerce p) (coerce l) . castPtr <*> peek
  s (V4 a b c d) = glProgramUniform4d (coerce p) (coerce l) a b c d

programUniform1i :: Program -> UniformLocation -> StateVar Int32
programUniform1i p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformiv (coerce p) (coerce l) . castPtr <*> peek
  s = glProgramUniform1i (coerce p) (coerce l)

programUniform2i :: Program -> UniformLocation -> StateVar (V2 Int32)
programUniform2i p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformiv (coerce p) (coerce l) . castPtr <*> peek
  s (V2 a b) = glProgramUniform2i (coerce p) (coerce l) a b

programUniform3i :: Program -> UniformLocation -> StateVar (V3 Int32)
programUniform3i p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformiv (coerce p) (coerce l) . castPtr <*> peek
  s (V3 a b c) = glProgramUniform3i (coerce p) (coerce l) a b c

programUniform4i :: Program -> UniformLocation -> StateVar (V4 Int32)
programUniform4i p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformiv (coerce p) (coerce l) . castPtr <*> peek
  s (V4 a b c d) = glProgramUniform4i (coerce p) (coerce l) a b c d

programUniform1ui :: Program -> UniformLocation -> StateVar Word32
programUniform1ui p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformuiv (coerce p) (coerce l) . castPtr <*> peek
  s = glProgramUniform1ui (coerce p) (coerce l)

programUniform2ui :: Program -> UniformLocation -> StateVar (V2 Word32)
programUniform2ui p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformuiv (coerce p) (coerce l) . castPtr <*> peek
  s (V2 a b) = glProgramUniform2ui (coerce p) (coerce l) a b

programUniform3ui :: Program -> UniformLocation -> StateVar (V3 Word32)
programUniform3ui p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformuiv (coerce p) (coerce l) . castPtr <*> peek
  s (V3 a b c) = glProgramUniform3ui (coerce p) (coerce l) a b c

programUniform4ui :: Program -> UniformLocation -> StateVar (V4 Word32)
programUniform4ui p l = StateVar g s where
  g = alloca $ (>>) <$> glGetUniformuiv (coerce p) (coerce l) . castPtr <*> peek
  s (V4 a b c d) = glProgramUniform4ui (coerce p) (coerce l) a b c d

--------------------------------------------------------------------------------
-- * Uniform Type
--------------------------------------------------------------------------------

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
