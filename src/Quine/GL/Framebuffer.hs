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
module Quine.GL.Framebuffer
  ( Framebuffer
  -- * Binding
  , boundFramebuffer
  -- * Attaching
  , framebufferTexture
  , framebufferRenderbuffer
  , framebufferTextureLayer
  -- * Completeness Check
  , checkFramebuffer
  -- * Framebuffer Targets
  , pattern DrawFramebuffer
  , pattern ReadFramebuffer
  , pattern RWFramebuffer
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
import Quine.GL.Renderbuffer
import Quine.GL.Texture

newtype Framebuffer = Framebuffer GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)
data FramebufferTarget = FramebufferTarget GLenum GLenum deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)
newtype FramebufferError = FramebufferError GLenum deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)
type FramebufferAttachmentPoint = GLenum

instance Object Framebuffer where
  object = coerce
  isa i = (GL_FALSE /=) `liftM` glIsFramebuffer (coerce i)
  deletes xs = liftIO $ allocaArray n $ \p -> do
    pokeArray p (coerce xs)
    glDeleteFramebuffers (fromIntegral n) p
    where n = length xs

instance Gen Framebuffer where
  gens n = liftIO $ allocaArray n $ \p -> do
    glGenFramebuffers (fromIntegral n) p
    map Framebuffer <$> peekArray n p

-- | The default Framebuffer is not just the null object but the screen buffer of the context
instance Default Framebuffer where
  def = Framebuffer 0

-- * Binding

boundFramebuffer :: FramebufferTarget -> StateVar Framebuffer
boundFramebuffer (FramebufferTarget target binding) = StateVar g s where
  g = do
    i <- alloca $ liftM2 (>>) (glGetIntegerv binding) peek
    return $ Framebuffer (fromIntegral i)
  s = glBindBuffer target . coerce

-- * Attaching Buffer

-- | Attach a 'Texture' to the currently bound 'Framebuffer'
framebufferTexture :: MonadIO m => FramebufferTarget -> FramebufferAttachmentPoint -> Texture -> MipmapLevel -> m ()
framebufferTexture (FramebufferTarget t _) slot tex = liftIO . glFramebufferTexture t slot (object tex)

-- | Attach a single layer of a 'Texture' to the currently bound 'Framebuffer'
-- also usable to attach a cube map 'Texture' or cube map texture array
-- For cube map textures, layer is translated into a cube map face according to: face = k `mod` 6.
-- For cube map array textures, layer is translated into an array layer and face according to: layer = ceil (layer / 6) and face = k `mod` 6
framebufferTextureLayer :: MonadIO m => FramebufferTarget -> FramebufferAttachmentPoint -> Texture -> MipmapLevel -> TextureLayer -> m ()
framebufferTextureLayer (FramebufferTarget t _) slot tex level = liftIO . glFramebufferTextureLayer t slot (object tex) level

framebufferRenderbuffer :: MonadIO m => FramebufferTarget -> FramebufferAttachmentPoint -> Renderbuffer -> m () 
framebufferRenderbuffer (FramebufferTarget t _) slot = liftIO . glFramebufferRenderbuffer t slot GL_RENDERBUFFER . object

checkFramebuffer :: MonadIO m => FramebufferTarget -> m (Maybe FramebufferError)
checkFramebuffer (FramebufferTarget t _) = do
  status <- glCheckFramebufferStatus t
  case status of
    GL_FRAMEBUFFER_COMPLETE -> return Nothing
    e -> return $ Just $ FramebufferError e

pattern DrawFramebuffer = FramebufferTarget GL_DRAW_FRAMEBUFFER GL_DRAW_FRAMEBUFFER_BINDING
pattern ReadFramebuffer = FramebufferTarget GL_READ_FRAMEBUFFER GL_READ_FRAMEBUFFER_BINDING
pattern RWFramebuffer   = FramebufferTarget GL_FRAMEBUFFER GL_FRAMEBUFFER_BINDING -- not sure if GL_FRAMEBUFFER_BINDING is still valid
