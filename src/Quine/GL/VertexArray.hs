{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.GL.VertexArray
  ( VertexArray(..)
  , boundVertexArray
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Data
import Data.Default
#if ! MIN_VERSION_base(4,8,0)
import Data.Functor
#endif
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Data.StateVar
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types
import Quine.GL.Object

newtype VertexArray = VertexArray GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

instance Object VertexArray where
  object = coerce
  isa i = (GL_FALSE /=) `liftM` glIsVertexArray (coerce i)
  deletes xs = liftIO $ allocaArray n $ \p -> do
    pokeArray p (coerce xs)
    glDeleteVertexArrays (fromIntegral n) p
    where n = length xs

instance Gen VertexArray where
  gens n = liftIO $ allocaArray n $ \p -> do
    glGenVertexArrays (fromIntegral n) p
    map VertexArray <$> peekArray n p

instance Default VertexArray where
  def = VertexArray 0

boundVertexArray :: StateVar VertexArray
boundVertexArray = StateVar g s where
  g = do
    i <- alloca $ liftM2 (>>) (glGetIntegerv GL_VERTEX_ARRAY_BINDING) peek
    return $ VertexArray (fromIntegral i)
  s = glBindVertexArray . coerce
