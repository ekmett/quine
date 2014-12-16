{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE LambdaCase         #-}
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
  , FramebufferAttachment(attach)
  , framebufferTexture
  , framebufferRenderbuffer
  , framebufferTextureLayer
  -- * Completeness Check
  , checkFramebufferStatus
  -- * Framebuffer Targets
  , FramebufferTarget(..)
  , pattern DrawFramebuffer
  , pattern ReadFramebuffer
  , pattern RWFramebuffer
  , FramebufferError(..)
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
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
newtype FramebufferError = FramebufferError GLenum deriving (Eq,Ord,Read,Typeable,Data,Generic)
type FramebufferAttachmentPoint = GLenum

instance Exception FramebufferError

instance Show FramebufferError where
  showsPrec d = \ case
    FramebufferError GL_FRAMEBUFFER_COMPLETE -> showString "GL_FRAMEBUFFER_COMPLETE"
    FramebufferError GL_FRAMEBUFFER_UNDEFINED -> showString "GL_FRAMEBUFFER_UNDEFINED"
    FramebufferError GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT -> showString "GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT"
    FramebufferError GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT -> showString "GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT"
    FramebufferError GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER -> showString "GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER"
    FramebufferError GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER -> showString "GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER"
    FramebufferError GL_FRAMEBUFFER_UNSUPPORTED -> showString "GL_FRAMEBUFFER_UNSUPPORTED"
    FramebufferError GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE -> showString "GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE"
    FramebufferError GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS -> showString "GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS"
    t -> showsPrec d t


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

class FramebufferAttachment a where
  attach :: MonadIO m => FramebufferTarget -> FramebufferAttachmentPoint -> a -> m ()

instance FramebufferAttachment Texture where
  attach target slot tex = framebufferTexture target slot tex 0

instance FramebufferAttachment (Renderbuffer a) where
  attach = framebufferRenderbuffer

-- * Binding

boundFramebuffer :: FramebufferTarget -> StateVar Framebuffer
boundFramebuffer (FramebufferTarget target binding) = StateVar g s where
  g = fmap (Framebuffer . fromIntegral) $ alloca $ liftM2 (>>) (glGetIntegerv binding) peek
  s = glBindFramebuffer target . coerce

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

framebufferRenderbuffer :: MonadIO m => FramebufferTarget -> FramebufferAttachmentPoint -> Renderbuffer a -> m () 
framebufferRenderbuffer (FramebufferTarget t _) slot = liftIO . glFramebufferRenderbuffer t slot GL_RENDERBUFFER . object

checkFramebufferStatus :: MonadIO m => FramebufferTarget -> m (Maybe FramebufferError)
checkFramebufferStatus (FramebufferTarget t _) = do
  status <- glCheckFramebufferStatus t
  case status of
    GL_FRAMEBUFFER_COMPLETE -> return Nothing
    e -> return $ Just $ FramebufferError e

pattern DrawFramebuffer = FramebufferTarget GL_DRAW_FRAMEBUFFER GL_DRAW_FRAMEBUFFER_BINDING
pattern ReadFramebuffer = FramebufferTarget GL_READ_FRAMEBUFFER GL_READ_FRAMEBUFFER_BINDING
pattern RWFramebuffer   = FramebufferTarget GL_FRAMEBUFFER GL_FRAMEBUFFER_BINDING -- not sure if GL_FRAMEBUFFER_BINDING is still valid
