{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.GL.Texture
  ( Texture
  , TextureWrapping
  , TextureMinificationFilter
  , TextureMagnificationFilter
  , MipmapLevel
  , TextureLayer
  -- * Texture Binding
  , boundTexture
  -- * Texture Targets
  , TextureTarget(..)
  , pattern Texture1D
  , pattern Texture1DArray
  , pattern Texture2D
  , pattern Texture2DArray
  , pattern Texture2DMultisample
  , pattern Texture2DMultisampleArray
  , pattern Texture3D
  , pattern TextureBuffer
  , pattern TextureCubeMap
  , pattern TextureRectangle
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Data
import Data.Default
import Data.Functor
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types
import Quine.GL.Object
import Quine.StateVar

data TextureTarget = TextureTarget GLenum GLenum
type TextureWrapping = GLenum
type TextureMinificationFilter = GLenum
type TextureMagnificationFilter = GLenum
type MipmapLevel = GLint
type TextureLayer = GLint

newtype Texture = Texture GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

instance Object Texture where
  object = coerce
  isa i = (GL_FALSE /=) `liftM` glIsTexture (coerce i)
  deletes xs = liftIO $ allocaArray n $ \p -> do
    pokeArray p (coerce xs)
    glDeleteTextures (fromIntegral n) p
    where n = length xs

instance Gen Texture where
  gens n = liftIO $ allocaArray n $ \p -> do
    glGenTextures (fromIntegral n) p
    map Texture <$> peekArray n p

instance Default Texture where
  def = Texture 0

boundTexture :: TextureTarget -> StateVar Texture
boundTexture (TextureTarget target binding) = StateVar g s where
  g = do
    i <- alloca $ liftM2 (>>) (glGetIntegerv binding) peek
    return $ Texture (fromIntegral i)
  s = glBindTexture target . coerce

pattern Texture1D = TextureTarget GL_TEXTURE_1D GL_TEXTURE_BINDING_1D
pattern Texture1DArray  = TextureTarget GL_TEXTURE_1D_ARRAY GL_TEXTURE_BINDING_1D_ARRAY
pattern Texture2D = TextureTarget GL_TEXTURE_2D GL_TEXTURE_BINDING_2D
pattern Texture2DArray  = TextureTarget GL_TEXTURE_2D_ARRAY GL_TEXTURE_BINDING_2D_ARRAY
pattern Texture2DMultisample  = TextureTarget GL_TEXTURE_2D_MULTISAMPLE GL_TEXTURE_BINDING_2D_MULTISAMPLE
pattern Texture2DMultisampleArray = TextureTarget GL_TEXTURE_2D_MULTISAMPLE_ARRAY GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY
pattern Texture3D = TextureTarget GL_TEXTURE_3D GL_TEXTURE_BINDING_3D
pattern TextureBuffer = TextureTarget GL_TEXTURE_BUFFER GL_TEXTURE_BINDING_BUFFER
pattern TextureCubeMap  = TextureTarget GL_TEXTURE_CUBE_MAP GL_TEXTURE_BINDING_CUBE_MAP
pattern TextureRectangle  = TextureTarget GL_TEXTURE_RECTANGLE GL_TEXTURE_BINDING_RECTANGLE
