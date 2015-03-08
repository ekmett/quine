{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- A Sampler stores sampling parameters for texture access in shaders
-- Requires: OpenGL 3.3+
-- OpenGL Wiki: <https://www.opengl.org/wiki/Sampler_Object>
--------------------------------------------------------------------
module Quine.GL.Sampler
  ( Sampler
  -- * Binding
  , boundSampler
  -- * Sampler Parameter
  -- $samplerParameter
  , SamplerParameter
  , samplerParameterf
  , samplerParameter2f
  , samplerParameter3f
  , samplerParameter4f
  , samplerParameterfv
  , samplerParameteri
  , samplerParameter2i
  , samplerParameter3i
  , samplerParameter4i
  , samplerParameteriv
  , samplerParameterIiv
  , samplerParameterIuiv
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Data
import Data.Int
import Data.Word
import Data.Default
import Linear
import Linear.V
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.Var
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types
import Quine.GL.Object
import Quine.GL.Texture (TextureUnit)

newtype Sampler = Sampler GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)
type SamplerParameter = GLenum

instance Object Sampler where
  object = coerce
  isa i = (GL_FALSE /=) `liftM` glIsSampler (coerce i)
  deletes xs = liftIO $ allocaArray n $ \p -> do
    pokeArray p (coerce xs)
    glDeleteSamplers (fromIntegral n) p
    where n = length xs

instance Gen Sampler where
  gens n = liftIO $ allocaArray n $ \p -> do
    glGenSamplers (fromIntegral n) p
    map Sampler <$> peekArray n p

instance Default Sampler where
  def = Sampler 0

-- * Binding

boundSampler :: TextureUnit -> Var Sampler
boundSampler u = Var g s where
  g = fmap (Sampler . fromIntegral) $ alloca $ liftM2 (>>) (glGetIntegerv GL_SAMPLER_BINDING) peek
  s = glBindSampler u . coerce

-- * Sampler Parameter

-- $samplerParameter
--
-- Using 'Sampler's is the prefered way to store 'TextureParameter' settings. When a sampler is bound to
-- a 'TextureUnit' _all_ settings tied to the 'Texture' are ignored.

samplerParameterf :: Sampler -> SamplerParameter -> Var Float
samplerParameterf sampler p = Var g s where
  g = alloca $ (>>) <$> glGetSamplerParameterfv (coerce sampler) p . castPtr <*> peek
  s = glSamplerParameterf (coerce sampler) p

samplerParameterfv' :: Storable (f Float) => Sampler -> SamplerParameter -> Var (f Float)
samplerParameterfv' sampler p = Var g s where
  g = alloca $ (>>) <$> glGetSamplerParameterfv (coerce sampler) p . castPtr <*> peek
  s v = alloca $ (>>) <$> glSamplerParameterfv (coerce sampler) p . castPtr <*> (`poke` v)

samplerParameterfv :: Dim n => Sampler -> SamplerParameter -> Var (V n Float)
samplerParameterfv = samplerParameterfv'

samplerParameter2f :: Sampler -> SamplerParameter -> Var (V2 Float)
samplerParameter2f = samplerParameterfv'

samplerParameter3f :: Sampler -> SamplerParameter -> Var (V3 Float)
samplerParameter3f = samplerParameterfv'

samplerParameter4f :: Sampler -> SamplerParameter -> Var (V4 Float)
samplerParameter4f = samplerParameterfv'

samplerParameteri :: Sampler -> SamplerParameter -> Var Int32
samplerParameteri sampler p = Var g s where
  g = alloca $ (>>) <$> glGetSamplerParameteriv (coerce sampler) p . castPtr <*> peek
  s = glSamplerParameteri (coerce sampler) p

samplerParameteriv' :: Storable (f Int32) => Sampler -> SamplerParameter -> Var (f Int32)
samplerParameteriv' sampler p = Var g s where
  g = alloca $ (>>) <$> glGetSamplerParameteriv (coerce sampler) p . castPtr <*> peek
  s v = alloca $ (>>) <$> glSamplerParameteriv (coerce sampler) p . castPtr <*> (`poke` v)

samplerParameteriv :: Dim n => Sampler -> SamplerParameter -> Var (V n Int32)
samplerParameteriv = samplerParameteriv'

samplerParameter2i :: Sampler -> SamplerParameter -> Var (V2 Int32)
samplerParameter2i = samplerParameteriv'

samplerParameter3i :: Sampler -> SamplerParameter -> Var (V3 Int32)
samplerParameter3i = samplerParameteriv'

samplerParameter4i :: Sampler -> SamplerParameter -> Var (V4 Int32)
samplerParameter4i = samplerParameteriv'

samplerParameterIiv :: Dim n => Sampler -> SamplerParameter -> Var (V n Int32)
samplerParameterIiv sampler p = Var g s where
  g = alloca $ (>>) <$> glGetSamplerParameterIiv (coerce sampler) p . castPtr <*> peek
  s v = alloca $ (>>) <$> glSamplerParameterIiv (coerce sampler) p . castPtr <*> (`poke` v)

samplerParameterIuiv :: Dim n => Sampler -> SamplerParameter -> Var (V n Word32)
samplerParameterIuiv sampler p = Var g s where
  g = alloca $ (>>) <$> glGetSamplerParameterIuiv (coerce sampler) p . castPtr <*> peek
  s v = alloca $ (>>) <$> glSamplerParameterIuiv (coerce sampler) p . castPtr <*> (`poke` v)

