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
    Attribute(..)
  -- * Attribute Location
  , AttributeLocation
  , attributeLocation
  -- * Attribute Definition
  , vertexAttribute
  -- * Attribute Pointer
  , vertexAttribPointerI
  , vertexAttribPointer
  -- * Layout
  , Layout(..)
  , BaseType, Components, Normalized, Stride, OffsetPtr
  -- * Layout Annotation
  , UnAnnotated(..), LayoutAnnotation(..)
  , HasLayoutAnnotation(..)
  
  , AsType(..)
  ) where

import Control.Monad.IO.Class
import Data.Word
import Data.Int
import Data.Data
import GHC.Generics
import Data.Foldable
import Data.Traversable
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Graphics.GL.Core41
import Graphics.GL.Types
import Quine.GL.Types
import Quine.GL.Program
import Quine.StateVar

--------------------------------------------------------------------------------
-- * Attribute Locations
--------------------------------------------------------------------------------

type AttributeLocation = GLint

attributeLocation :: MonadIO m => Program -> String -> m AttributeLocation
attributeLocation (Program p) s = liftIO $ withCString s (glGetAttribLocation p . castPtr)

--------------------------------------------------------------------------------
-- * Layout Definition
--------------------------------------------------------------------------------

type BaseType   = GLenum
type Components = Int
type Stride     = Int
type Normalized = Bool
type OffsetPtr  = Ptr ()
data AsType     = AsInteger | AsFloating

data Layout     = Layout Components BaseType Normalized Stride OffsetPtr
  deriving (Data,Typeable,Generic,Eq,Ord,Show)

newtype UnAnnotated a = UnAnnotated { unAnnotate :: a }
  deriving (Data,Typeable,Generic,Functor,Foldable,Traversable,Eq,Ord,Show,Read)
newtype LayoutAnnotation a = LayoutAnnotation { getLayout :: Layout }
  deriving (Data,Typeable,Generic,Eq,Ord,Show)

instance Storable a => Storable (UnAnnotated a) where
  sizeOf _ = sizeOf (undefined::a)
  alignment _ = alignment (undefined::a)
  peek = fmap UnAnnotated . peek . castPtr
  poke ptr = poke (castPtr ptr) . unAnnotate

class HasLayoutAnnotation a where
  -- | Annotates any proxy 'p' of an attribute 'a' with memory 'Layout' informations 
  layoutAnnotation :: Functor p => p (a UnAnnotated) -> a LayoutAnnotation

type AttributeAccessor a b = a LayoutAnnotation -> LayoutAnnotation b

-- | Associates the vertex attribute to the data in the vertex buffer. 
-- The association is stored in the vertex array object 'OpenGL' wise, so a VAO must be bound
vertexAttribute :: HasLayoutAnnotation a => AttributeLocation -> SettableStateVar (Maybe (AttributeAccessor a b))
vertexAttribute l = SettableStateVar $ \case
  Nothing -> glDisableVertexAttribArray (fromIntegral l)
  Just accessor -> do
    glEnableVertexAttribArray (fromIntegral l)
    vertexAttribPointer l (getLayout . accessor $ layoutAnnotation (Proxy::Proxy a))
      

-- | A not so high level binding to 'glVertexAttribIPointer'
-- sets the attribute array pointer for an integer attribute (no conversion)
vertexAttribPointerI :: MonadIO m => AttributeLocation -> Layout -> m ()
vertexAttribPointerI loc (Layout comp ty _norm stride offPtr) = 
  liftIO $ glVertexAttribIPointer (fromIntegral loc) (fromIntegral comp) ty (fromIntegral stride) offPtr
                                  -- ^ why is coerce here not possible (like in Quine/GL/Uniform.hs#L112)?

-- | A not so high level binding to 'glVertexAttribPointer'
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

instance Attribute a => Attribute (UnAnnotated a) where
  components _ = components (Proxy::Proxy a)
  baseType _ = baseType (Proxy::Proxy a)

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
