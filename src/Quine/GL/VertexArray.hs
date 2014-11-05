{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Quine.GL.VertexArray
  ( VertexArray(..)
  , boundVertexArray
  ) where

import Control.Monad.IO
import Data.Coerce
import Data.Data
import Data.Default
import Data.Functor
import Foreign.Marshal.Alloc
import GHC.Generics
import Quine.StateVar
import Graphics.GL.Raw.Extension.ARB.VertexArrayObject
import Graphics.GL.Raw.Profile.Core45

newtype VertexArray = VertexArray GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

instance Object VertexArray where
  objectId = coerce
  isa i = (GL_FALSE /=) `liftM` glIsVertexArray (coerce i)
  deletes xs = allocaArray n $ \p -> do
    pokeArray p (coerce xs)
    glDeleteVertexArrays (fromIntegral n) p
    where n = length xs

instance Gen VertexArray where
  creates n = liftIO $ allocaArray n $ \p -> 
    glGenVertexArrays (fromIntegral n)
    coerce <$> peekArray n p

instance Default VertexArray where
  def = VertexArray 0

boundVertexArray :: StateVar VertexArray
boundVertexArray = StateVar g s where
  g = do
    i <- alloca $ liftM (>>) (glGetIntegerv GL_VERTEX_ARRAY_BINDING) peek
    return $ VertexArray (fromIntegral i)
  s = coerce glBindVertexArray
