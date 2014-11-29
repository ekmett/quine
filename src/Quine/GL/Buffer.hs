{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- OpenGL Doc: <https://www.opengl.org/sdk/docs/man/html/glBindBuffer.xhtml>
--
-- Also usable for bindless rendering:
-- <https://www.opengl.org/discussion_boards/showthread.php/170388-Bindless-Stuff?p=1199088&viewfull=1#post1199088>
--------------------------------------------------------------------
module Quine.GL.Buffer
  ( Buffer(..)
  , boundBufferAt
  -- * Buffer Data
  , BufferData(..)
  , bufferData
  , bufferDataDirect
  -- * Buffer Targets
  , BufferTarget(..)
  , pattern ArrayBuffer
  , pattern AtomicCounterBuffer
  , pattern CopyReadBuffer
  , pattern CopyWriteBuffer
  , pattern DispatchIndirectBuffer
  , pattern DrawIndirectBuffer
  , pattern ElementArrayBuffer
  , pattern PixelPackBuffer
  , pattern PixelUnpackBuffer
  , pattern QueryBuffer
  , pattern ShaderStorageBuffer
  , pattern TextureBuffer
  , pattern TransformFeedbackBuffer
  , pattern UniformBuffer

  -- * Buffer Usage
  , BufferUsage(..)
  -- * Usage Types
  -- $usage
  -- $stream
  , pattern StreamDraw
  , pattern StreamRead
  , pattern StreamCopy
  -- $static
  , pattern StaticDraw
  , pattern StaticRead
  , pattern StaticCopy
  -- $dynamic
  , pattern DynamicDraw
  , pattern DynamicRead
  , pattern DynamicCopy
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.Coerce
import Data.Data
import Data.Default
import qualified Data.Vector.Storable as V
import Data.Functor
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Ext.EXT.DirectStateAccess
import Graphics.GL.Types
import Quine.StateVar
import Quine.GL.Object

-- | A 'Buffer' is the generic OpenGL storage object for multiple possible kind of data
--
-- For ArrayBuffer it storages vertex attributes like position, normal or color an provides
-- the MD (Multiple Data) in SIMD (Single Instruction, Multiple Data)
newtype Buffer a = Buffer GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

data BufferTarget = BufferTarget GLenum GLenum deriving (Typeable,Data,Generic)

newtype BufferUsage = BufferUsage GLenum deriving (Eq,Num,Show,Typeable,Data,Generic)

data BufferException = BufferException String deriving (Show,Typeable)
instance Exception BufferException

instance Object (Buffer a) where
  object = coerce
  isa i = (GL_FALSE /=) `liftM` glIsBuffer (coerce i)
  deletes xs = liftIO $ allocaArray n $ \p -> do
    pokeArray p (coerce xs)
    glDeleteBuffers (fromIntegral n) p
    where n = length xs

instance Gen (Buffer a) where
  gens n = liftIO $ allocaArray n $ \p -> do
    glGenBuffers (fromIntegral n) p
    map Buffer <$> peekArray n p

instance Default (Buffer a) where
  def = Buffer 0

-- * Buffer Data

class BufferData a where
  -- | perfom a monadic action with the pointer to the raw content and the number of elements
  withRawData :: MonadIO m => a -> (Ptr () -> IO b) -> m b
  -- | reads 'a' from a pointer and the given size of a in bytes 
  fromRawData :: MonadIO m => Int -> Ptr () -> m a
  -- | size of the complete data in bytes
  sizeOfData :: a -> Int

-- | This instance writes the data interleaved because the 'Vector' structure is already interleaved.
-- If you want an different layout use a newtype wrapper or an own data structure.
instance Storable a => BufferData (V.Vector a) where
  withRawData v m = liftIO $ V.unsafeWith v $ m . castPtr
  fromRawData bytes ptr = liftIO $ do
    fp <- newForeignPtr_ $ castPtr ptr
    return $ V.unsafeFromForeignPtr0 fp (bytes `div` sizeOf (undefined::a))
  sizeOfData v = V.length v * sizeOf (undefined::a)

instance Storable a => BufferData [a] where
  withRawData v m = liftIO . withArray v $ m . castPtr
  fromRawData bytes = liftIO . peekArray (bytes `div` sizeOf (undefined::a)) . castPtr
  sizeOfData v = length v * sizeOf (undefined::a)

-- * Buffer Access

boundBufferAt :: BufferTarget -> StateVar (Buffer a)
boundBufferAt (BufferTarget target binding) = StateVar g s where
  g = do
    i <- alloca $ liftM2 (>>) (glGetIntegerv binding) peek
    return $ Buffer (fromIntegral i)
  s = glBindBuffer target . coerce

-- | bindless uploading data to the argumented buffer (since OpenGL 4.4+ or with 'gl_EXT_direct_state_access')
bufferDataDirect :: forall a. BufferData a => Buffer a -> StateVar (BufferUsage, a)
bufferDataDirect (Buffer i)
  | gl_EXT_direct_state_access = StateVar g s
  | otherwise = throw $ BufferException "gl_EXT_direct_state_access unsupported" where
  g = alloca $ \sizePtr ->
      alloca $ \usagePtr -> do
        glGetNamedBufferParameterivEXT i GL_BUFFER_SIZE sizePtr
        glGetNamedBufferParameterivEXT i GL_BUFFER_USAGE usagePtr
        usage <- peek usagePtr
        size  <- peek sizePtr
        allocaBytes (fromIntegral size) $ \rawPtr -> do
          glGetNamedBufferSubDataEXT i 0 (fromIntegral size) (castPtr rawPtr)
          (BufferUsage $ fromIntegral usage,) <$> fromRawData (fromIntegral size) rawPtr
  s (u,v) = withRawData v $ \ptr -> glNamedBufferDataEXT i (fromIntegral $ sizeOfData v) ptr (coerce u)

-- | uploading data to the currently at 'BufferTarget' bound buffer
bufferData :: forall a. BufferData a => BufferTarget -> StateVar (BufferUsage, a)
bufferData (BufferTarget t _) = StateVar g s where
  g = alloca $ \sizePtr ->
      alloca $ \usagePtr -> do
        glGetBufferParameteriv t GL_BUFFER_SIZE sizePtr
        glGetBufferParameteriv t GL_BUFFER_USAGE usagePtr
        usage <- BufferUsage . fromIntegral <$> peek usagePtr
        size  <- peek sizePtr
        allocaBytes (fromIntegral size) $ \rawPtr -> do
          glGetBufferSubData t 0 (fromIntegral size) (castPtr rawPtr)
          (usage,) <$> fromRawData (fromIntegral size) rawPtr
  s (u,v) = withRawData v $ \ptr -> glBufferData t (fromIntegral $ sizeOfData v) ptr (coerce u)

-- * Buffer Types

-- | Vertex attributes
pattern ArrayBuffer = BufferTarget GL_ARRAY_BUFFER GL_ARRAY_BUFFER_BINDING

-- | Atomic counter storage
pattern AtomicCounterBuffer = BufferTarget GL_ATOMIC_COUNTER_BUFFER GL_ATOMIC_COUNTER_BUFFER_BINDING

-- | Buffer copy source
pattern CopyReadBuffer = BufferTarget GL_COPY_READ_BUFFER GL_COPY_READ_BUFFER_BINDING

-- | Buffer copy destination
pattern CopyWriteBuffer = BufferTarget GL_COPY_WRITE_BUFFER GL_COPY_WRITE_BUFFER_BINDING

-- | Indirect compute dispatch commands
pattern DispatchIndirectBuffer = BufferTarget GL_DISPATCH_INDIRECT_BUFFER GL_DISPATCH_INDIRECT_BUFFER_BINDING

-- | Indirect command arguments
pattern DrawIndirectBuffer = BufferTarget GL_DRAW_INDIRECT_BUFFER GL_DRAW_INDIRECT_BUFFER_BINDING

-- | Vertex array indices
pattern ElementArrayBuffer = BufferTarget GL_ELEMENT_ARRAY_BUFFER GL_ELEMENT_ARRAY_BUFFER_BINDING

-- | Pixel read target
pattern PixelPackBuffer = BufferTarget GL_PIXEL_PACK_BUFFER GL_PIXEL_PACK_BUFFER_BINDING

-- | Texture data source
pattern PixelUnpackBuffer = BufferTarget GL_PIXEL_UNPACK_BUFFER GL_PIXEL_UNPACK_BUFFER_BINDING

-- | Query result buffer
pattern QueryBuffer = BufferTarget GL_QUERY_BUFFER GL_QUERY_BUFFER_BINDING

-- | Shader storage buffers
--
-- You should probably use the 'Quine.GL.Block.STD140' or 'Quine.GL.Block.STD430' newtype wrapper around the contents.
-- Requires OpenGL 4.3+
pattern ShaderStorageBuffer = BufferTarget GL_SHADER_STORAGE_BUFFER GL_SHADER_STORAGE_BUFFER_BINDING

-- | Texture data buffer
pattern TextureBuffer = BufferTarget GL_TEXTURE_BUFFER GL_TEXTURE_BUFFER_BINDING

-- | Transform feedback buffer
pattern TransformFeedbackBuffer = BufferTarget GL_TRANSFORM_FEEDBACK_BUFFER GL_TRANSFORM_FEEDBACK_BUFFER_BINDING

-- | Uniform block storage
--
-- You should probably use the 'Quine.GL.Block.STD140' newtype wrapper around the contents.
pattern UniformBuffer = BufferTarget GL_UNIFORM_BUFFER GL_UNIFORM_BUFFER_BINDING

-- * Usage

-- $usage
--
-- Terminology:
--
-- [Stream] The data store contents will be modified once and used at most a few times.
--
-- [Static] The data store contents will be modified once and used many times.
--
-- [Dynamic] The data store contents will be modified repeatedly and used many times.
--
-- [Draw] The data store contents are modified by the application, and used as the source for GL drawing and image specification commands.
--
-- [Read] The data store contents are modified by reading data from the GL, and used to return that data when queried by the application.
--
-- [Copy] The data store contents are modified by reading data from the GL, and used as the source for GL drawing and image specification commands.

pattern StreamDraw = BufferUsage GL_STREAM_DRAW
pattern StreamRead = BufferUsage GL_STREAM_READ
pattern StreamCopy = BufferUsage GL_STREAM_COPY
pattern StaticDraw = BufferUsage GL_STATIC_DRAW
pattern StaticRead = BufferUsage GL_STATIC_READ
pattern StaticCopy = BufferUsage GL_STATIC_COPY
pattern DynamicDraw = BufferUsage GL_DYNAMIC_DRAW
pattern DynamicRead = BufferUsage GL_DYNAMIC_READ
pattern DynamicCopy = BufferUsage GL_DYNAMIC_COPY
