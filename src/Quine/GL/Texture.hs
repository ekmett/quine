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
  , TextureTarget, TextureBinding
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

type TextureTarget = GLenum
type TextureBinding = GLenum
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

boundTexture :: TextureTarget -> TextureBinding -> StateVar Texture
boundTexture target binding = StateVar g s where
  g = do
    i <- alloca $ liftM2 (>>) (glGetIntegerv binding) peek
    return $ Texture (fromIntegral i)
  s = glBindTexture target . coerce
