{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns       #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- A cube with 6 texture images assigned to each face
--------------------------------------------------------------------
module Quine.Cubemap
  ( -- * A Cube of Images
    Cubemap(..)
  , GLFaceTargets
  , glFaceTargets
  ) where

import Control.Monad
import Data.Data
import Data.Foldable
import Data.Traversable
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types
import Quine.Image

-- | Faces in OpenGL order
data Cubemap a = Cubemap
  { faceRight   :: !a
  -- ^ GL_TEXTURE_CUBE_MAP_POSITIVE_X
  , faceLeft    :: !a
  -- ^ GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  , faceTop     :: !a
  -- ^ GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  , faceBottom  :: !a
  -- ^ GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  , faceFront   :: !a
  -- ^ GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  , faceBack    :: !a
  -- ^ GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
  } deriving ( Show,Functor,Foldable,Traversable,Data,Typeable,Generic )

type GLFaceTargets = Cubemap GLenum

instance Image2D i => Image2D (Cubemap i) where
  upload cube _ l = zipWithM_ (\img t -> upload img t l) (toList cube) (toList glFaceTargets)
  store cube@Cubemap{faceRight} t = do
    store faceRight GL_TEXTURE_CUBE_MAP
    upload cube t 0

glFaceTargets :: GLFaceTargets
glFaceTargets = Cubemap 
  GL_TEXTURE_CUBE_MAP_POSITIVE_X GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z GL_TEXTURE_CUBE_MAP_NEGATIVE_X

