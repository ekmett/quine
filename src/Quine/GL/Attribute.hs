{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE RecordWildCards        #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- OpenGL Attribute
--------------------------------------------------------------------
module Quine.GL.Attribute
  ( 
  -- * Attribute Location
    AttributeLocation
  , attributeLocation
  , vertexAttribPointerI
  , vertexAttribPointer
  
  -- * Attribute Stream To Buffer
  , writeBufferData
  , assignAttribute
  , AsType(..)

  -- * Base Types
  , BaseType
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Data
import Data.Coerce
import Data.Foldable
import Foreign.Storable
import Foreign.Marshal.Utils
import Data.Word
import Data.Int
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Graphics.GL.Core41
import Graphics.GL.Types
import Quine.GL.Program
import Quine.GL.Buffer
import Quine.GL.Types

--------------------------------------------------------------------------------
-- * Attribute Locations
--------------------------------------------------------------------------------

type AttributeLocation = GLint

attributeLocation :: MonadIO m => Program -> String -> m AttributeLocation
attributeLocation (Program p) s = liftIO $ withCString s (glGetAttribLocation p . castPtr)

type BaseType = GLenum
type Components = Int
type Stride = Int
type Normalized = Bool
type OffsetPtr = Ptr ()
data AsType = AsInteger | AsFloating
data Layout = Layout Components BaseType Normalized Stride OffsetPtr

data BufferBackend = BufferBackend
  { bufferedData :: Ptr ()
  -- ^ written raw data
  , bytesWritten :: Int
  -- ^ simple byte counter to keep track of the bytes stored in the Ptr
  , attributeLayout :: [(AttributeLocation, AsType, Layout)] 
  -- ^ maps for each written attribute the memory layout
  }

emptyBufferBackend :: BufferBackend
emptyBufferBackend = BufferBackend nullPtr 0 []

type AttributeWriter = StateT BufferBackend IO ()

-- | assign a stream of data to an 'AttributeLocation' in a 'Program'.
-- with 'glGetVertexAttrib' its possible to form a 'StateVar' but a bound buffer is necessary :( 
-- some day it could be a full 'StateVar' with with bindless
assignAttribute :: forall a f. (Storable a, BufferData (f a), Attribute a) => AttributeLocation -> AsType -> f a -> AttributeWriter
assignAttribute loc asType dat = do
  BufferBackend{..} <- get
  let newSize = bytesWritten + sizeOfData dat
  put =<< (withRawData dat $ \inPtr -> do
    ptr <- do reallocBytes bufferedData newSize -- possible source of fragmentation?
    copyBytes (ptr `plusPtr` bytesWritten) inPtr (sizeOfData dat)
    return $ BufferBackend ptr newSize ((loc, asType, attribLayout (Proxy :: Proxy a) 0 (nullPtr `plusPtr` bytesWritten)):attributeLayout))
  where
  attribLayout p = Layout (components p) (baseType p) (normalize p)

writeBufferData :: MonadIO m => BufferTarget -> BufferUsage -> AttributeWriter -> m ()
writeBufferData (BufferTarget t _) (BufferUsage u) w = liftIO $ do
  BufferBackend{..} <- execStateT w emptyBufferBackend
  -- push the pointer to the buffer
  glBufferData t (fromIntegral $ bytesWritten) bufferedData (coerce u)
  free bufferedData
  forM_ attributeLayout $ \case
    (loc, AsInteger, layout)  -> vertexAttribPointerI loc layout
    (loc, AsFloating, layout) -> vertexAttribPointer loc layout

-- | a not so low level binding to 'glVertexAttribIPointer'
-- sets the attribute array pointer for an integer attribute (no conversion)
vertexAttribPointerI :: MonadIO m => AttributeLocation -> Layout -> m ()
vertexAttribPointerI loc (Layout comp ty _norm stride offPtr) = 
  liftIO $ glVertexAttribIPointer (fromIntegral loc) (fromIntegral comp) ty (fromIntegral stride) offPtr
                                  -- ^ why is coerce here not possible (like in Quine/GL/Uniform.hs)?

-- | a not so low level binding to 'glVertexAttribPointer'
-- sets the attribute array pointer for an integer or floating attribute (integers are converted to the floating type)
vertexAttribPointer :: MonadIO m => AttributeLocation -> Layout -> m ()
vertexAttribPointer loc (Layout comp ty norm stride offPtr) =
  liftIO $ glVertexAttribPointer (fromIntegral loc) (fromIntegral comp) ty (if norm then GL_TRUE else GL_FALSE) (fromIntegral stride) offPtr

--------------------------------------------------------------------------------
-- * Attribute Definition
--------------------------------------------------------------------------------

class Attribute a where
  -- | 1, 2, 3 or 4 supported
  components :: p a -> Int
  baseType :: p a -> BaseType
  -- | specifies whether integer data values should be normalized ('True') 
  -- or converted directly as float values ('False') when they are accessed
  normalize :: p a -> Bool
  normalize _ = False

instance Attribute Float where
  components _ = 1
  baseType _ = GL_FLOAT

instance Attribute Double where
  components _ = 1
  baseType _ = GL_DOUBLE

instance Attribute Int32 where
  components _ = 1
  baseType _ = GL_INT

instance Attribute Int16 where
  components _ = 1
  baseType _ = GL_SHORT

instance Attribute Int8 where
  components _ = 1
  baseType _ = GL_BYTE

instance Attribute Word32 where
  components _ = 1
  baseType _ = GL_UNSIGNED_INT

instance Attribute Word16 where
  components _ = 1
  baseType _ = GL_UNSIGNED_SHORT

instance Attribute Word8 where
  components _ = 1
  baseType _ = GL_UNSIGNED_BYTE

instance Attribute Vec2 where
  components _ = 2
  baseType _ = GL_FLOAT

instance Attribute Vec3 where
  components _ = 3
  baseType _ = GL_FLOAT

instance Attribute Vec4 where
  components _ = 4
  baseType _ = GL_FLOAT
