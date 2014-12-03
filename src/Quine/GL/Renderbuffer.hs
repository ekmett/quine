{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE PatternSynonyms    #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- So called RenderTargets or used for "Render to Texture". 
-- Memory to render into, usable as a texture or to read
-- otherwise from it. 
--------------------------------------------------------------------
module Quine.GL.Renderbuffer
  ( Renderbuffer
  , RenderbufferTargeting(..)
  , pattern RenderbufferTarget
  , boundRenderbuffer
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
import Quine.StateVar
import Quine.GL.Object

newtype Renderbuffer = Renderbuffer GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)
data RenderbufferTargeting = RenderbufferTargeting GLenum GLenum deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

instance Object Renderbuffer where
  object = coerce
  isa i = (GL_FALSE /=) `liftM` glIsRenderbuffer (coerce i)
  deletes xs = liftIO $ allocaArray n $ \p -> do
    pokeArray p (coerce xs)
    glDeleteRenderbuffers (fromIntegral n) p
    where n = length xs

instance Gen Renderbuffer where
  gens n = liftIO $ allocaArray n $ \p -> do
    glGenRenderbuffers (fromIntegral n) p
    map Renderbuffer <$> peekArray n p

instance Default Renderbuffer where
  def = Renderbuffer 0

boundRenderbuffer :: RenderbufferTargeting -> StateVar Renderbuffer
boundRenderbuffer (RenderbufferTargeting target binding) = StateVar g s where
  g = do
    i <- alloca $ liftM2 (>>) (glGetIntegerv binding) peek
    return $ Renderbuffer (fromIntegral i)
  s = glBindBuffer target . coerce


pattern RenderbufferTarget = RenderbufferTargeting GL_RENDERBUFFER GL_RENDERBUFFER_BINDING
