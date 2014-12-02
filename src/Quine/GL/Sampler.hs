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
  -- * Sampler Parameter
  , SamplerParameter
  , samplerParameterf
  , samplerParameter2f
  , samplerParameter3f
  , samplerParameter4f
  , samplerParameteri
  , samplerParameter2i
  , samplerParameter3i
  , samplerParameter4i
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
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types
import Quine.GL.Object
import Quine.StateVar

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

-- * Sampler Parameter

samplerParameterf :: Sampler -> SamplerParameter -> StateVar Float
samplerParameterf sampler p = StateVar g s where
  g = alloca $ (>>) <$> glGetSamplerParameterfv (coerce sampler) p . castPtr <*> peek
  s = glSamplerParameterf (coerce sampler) p

samplerParameterfv' :: Storable (f Float) => Sampler -> SamplerParameter -> StateVar (f Float)
samplerParameterfv' sampler p = StateVar g s where
  g = alloca $ (>>) <$> glGetSamplerParameterfv (coerce sampler) p . castPtr <*> peek
  s v = alloca $ (>>) <$> glSamplerParameterfv (coerce sampler) p . castPtr <*> (`poke` v)

samplerParameter2f :: Sampler -> SamplerParameter -> StateVar (V2 Float)
samplerParameter2f = samplerParameterfv'

samplerParameter3f :: Sampler -> SamplerParameter -> StateVar (V3 Float)
samplerParameter3f = samplerParameterfv'

samplerParameter4f :: Sampler -> SamplerParameter -> StateVar (V4 Float)
samplerParameter4f = samplerParameterfv'

samplerParameteri :: Sampler -> SamplerParameter -> StateVar Int32
samplerParameteri sampler p = StateVar g s where
  g = alloca $ (>>) <$> glGetSamplerParameteriv (coerce sampler) p . castPtr <*> peek
  s = glSamplerParameteri (coerce sampler) p

samplerParameteriv' :: Storable (f Int32) => Sampler -> SamplerParameter -> StateVar (f Int32)
samplerParameteriv' sampler p = StateVar g s where
  g = alloca $ (>>) <$> glGetSamplerParameteriv (coerce sampler) p . castPtr <*> peek
  s v = alloca $ (>>) <$> glSamplerParameteriv (coerce sampler) p . castPtr <*> (`poke` v)

samplerParameter2i :: Sampler -> SamplerParameter -> StateVar (V2 Int32)
samplerParameter2i = samplerParameteriv'

samplerParameter3i :: Sampler -> SamplerParameter -> StateVar (V3 Int32)
samplerParameter3i = samplerParameteriv'

samplerParameter4i :: Sampler -> SamplerParameter -> StateVar (V4 Int32)
samplerParameter4i = samplerParameteriv'
