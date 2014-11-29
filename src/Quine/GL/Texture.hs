{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.GL.Texture
  ( Texture
  , TextureTarget
  , TextureWrapping
  , TextureMinificationFilter
  , TextureMagnificationFilter
  , MipmapLevel
  , TextureLayer
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import Data.Default
import Data.Functor
import Data.Coerce
import Foreign.Marshal.Array
import GHC.Generics
import Quine.GL.Object

import Graphics.GL.Core45
import Graphics.GL.Types

type TextureTarget = GLenum
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
