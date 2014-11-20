{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Jan-Philip Loos <jloos@maxdaten.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- OpenGL Doc: https://www.opengl.org/sdk/docs/man/html/glBindBuffer.xhtml
--
-- Also usable for bindless rendering:
-- https://www.opengl.org/discussion_boards/showthread.php/170388-Bindless-Stuff
--------------------------------------------------------------------
module Quine.GL.Buffer
  ( Buffer(..), boundBufferAt,
  -- * Buffer Data
    BufferData(..), bufferData
  -- * Buffer Targets
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
  , pattern StreamDraw
  , pattern StreamRead
  , pattern StreamCopy
  , pattern StaticDraw
  , pattern StaticRead
  , pattern StaticCopy
  , pattern DynamicDraw
  , pattern DynamicRead
  , pattern DynamicCopy
  ) where

import Control.Monad
import Control.Monad.IO.Class
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
import Graphics.GL.Types
import Quine.StateVar
import Quine.GL.Object

-- | A @'Buffer' is the generic OpenGL storage object for multiple possible kind of data
--
-- For ArrayBuffer it storages vertex attributes like position, normal or color an provides
-- the MD (Multiple Data) in SIMD (Single Instruction, Multiple Data)
newtype Buffer a = Buffer GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)
newtype BufferTarget = BufferTarget (GLenum, GLenum) deriving (Typeable,Data,Generic)
newtype BufferUsage = BufferUsage GLenum deriving (Typeable,Data,Generic)

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

-- | i bet there is already a package with
class BufferData a where
    -- | perfom a monadic action with the pointer to the raw content and the size of it in bytes
    withRawData :: a -> (Int -> Ptr () -> IO ()) -> IO ()
    fromRawData :: Int -> Ptr () -> IO a


instance Storable a => BufferData (V.Vector a) where
    withRawData v m = V.unsafeWith v $ m (sizeOf (undefined::a) * V.length v) . castPtr
    fromRawData bytes ptr = do
      fp <- newForeignPtr_ $ castPtr ptr
      return $ V.unsafeFromForeignPtr fp 0 (bytes `div` sizeOf (undefined::a))


instance Storable a => BufferData [a] where
    withRawData v m = withArrayLen v $ \n -> m (n * sizeOf (undefined::a)) . castPtr
    fromRawData = undefined

-- * Buffer Access

boundBufferAt :: BufferTarget -> StateVar (Buffer a)
boundBufferAt (BufferTarget (target, binding)) = StateVar g s where
  g = do
    i <- alloca $ liftM2 (>>) (glGetIntegerv binding) peek
    return $ Buffer (fromIntegral i)
  s = glBindBuffer target . coerce


-- | bindless uploading data to the argumented buffer
bufferData :: forall a. BufferData a => Buffer a -> StateVar (BufferUsage, a)
bufferData (Buffer i) = StateVar g s where
  g = alloca $ \sizePtr ->
      alloca $ \usagePtr -> do
      alloca $ \rawPtr -> do
          glGetNamedBufferParameteriv i GL_BUFFER_SIZE sizePtr
          glGetNamedBufferParameteriv i GL_BUFFER_USAGE usagePtr
          glGetNamedBufferPointerv i GL_BUFFER_MAP_POINTER rawPtr
          usage <- peek usagePtr
          size  <- peek sizePtr
          raw   <- peek rawPtr
          (BufferUsage $ fromIntegral usage,) <$> fromRawData (fromIntegral size) raw
  s (u,v) = withRawData v $ \size ptr -> glNamedBufferData i (fromIntegral size) ptr (coerce u)

-- | Vertex attributes
pattern ArrayBuffer = BufferTarget (GL_ARRAY_BUFFER, GL_ARRAY_BUFFER_BINDING)
-- | Atomic counter storage
pattern AtomicCounterBuffer = BufferTarget (GL_ATOMIC_COUNTER_BUFFER, GL_ATOMIC_COUNTER_BUFFER_BINDING)
-- | Buffer copy source
pattern CopyReadBuffer = BufferTarget (GL_COPY_READ_BUFFER, GL_COPY_READ_BUFFER_BINDING)
-- | Buffer copy destination
pattern CopyWriteBuffer = BufferTarget (GL_COPY_WRITE_BUFFER, GL_COPY_WRITE_BUFFER_BINDING)
-- | Indirect compute dispatch commands
pattern DispatchIndirectBuffer = BufferTarget (GL_DISPATCH_INDIRECT_BUFFER, GL_DISPATCH_INDIRECT_BUFFER_BINDING)
-- | Indirect command arguments
pattern DrawIndirectBuffer = BufferTarget (GL_DRAW_INDIRECT_BUFFER, GL_DRAW_INDIRECT_BUFFER_BINDING)
-- | Vertex array indices
pattern ElementArrayBuffer = BufferTarget (GL_ELEMENT_ARRAY_BUFFER, GL_ELEMENT_ARRAY_BUFFER_BINDING)
-- | Pixel read target
pattern PixelPackBuffer = BufferTarget (GL_PIXEL_PACK_BUFFER, GL_PIXEL_PACK_BUFFER_BINDING)
-- | Texture data source
pattern PixelUnpackBuffer = BufferTarget (GL_PIXEL_UNPACK_BUFFER, GL_PIXEL_UNPACK_BUFFER_BINDING)
-- | Query result buffer
pattern QueryBuffer = BufferTarget (GL_QUERY_BUFFER, GL_QUERY_BUFFER_BINDING)
-- | Read-write storage for shaders
pattern ShaderStorageBuffer = BufferTarget (GL_SHADER_STORAGE_BUFFER, GL_SHADER_STORAGE_BUFFER_BINDING)
-- | Texture data buffer
pattern TextureBuffer = BufferTarget (GL_TEXTURE_BUFFER, GL_TEXTURE_BUFFER_BINDING)
-- | Transform feedback buffer
pattern TransformFeedbackBuffer = BufferTarget (GL_TRANSFORM_FEEDBACK_BUFFER, GL_TRANSFORM_FEEDBACK_BUFFER_BINDING)
-- | Uniform block storage
pattern UniformBuffer = BufferTarget (GL_UNIFORM_BUFFER, GL_UNIFORM_BUFFER_BINDING)


-- | Draw: The data store contents are modified by the application, and used as the source for GL drawing and image specification commands.
-- | Read: The data store contents are modified by reading data from the GL, and used to return that data when queried by the application.
-- | Copy: The data store contents are modified by reading data from the GL, and used as the source for GL drawing and image specification commands.

-- | The data store contents will be modified once and used at most a few times.
pattern StreamDraw = GL_STREAM_DRAW
pattern StreamRead = GL_STREAM_READ
pattern StreamCopy = GL_STREAM_COPY
-- | The data store contents will be modified once and used many times.
pattern StaticDraw = GL_STATIC_DRAW
pattern StaticRead = GL_STATIC_READ
pattern StaticCopy = GL_STATIC_COPY
-- | The data store contents will be modified repeatedly and used many times.
pattern DynamicDraw = GL_DYNAMIC_DRAW
pattern DynamicRead = GL_DYNAMIC_READ
pattern DynamicCopy = GL_DYNAMIC_COPY
