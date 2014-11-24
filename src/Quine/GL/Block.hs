{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- OpenGL STD140 and STD430 support
--
-- Note STD430 can only be used for shader storage blocks, NOT
-- uniform blocks!
--------------------------------------------------------------------
module Quine.GL.Block
  ( Offset(..)
  , Block(..)
  , GBlock(..)
  , STD140(..)
  , STD430(..)
  ) where

import Control.Applicative
import Control.Category
import Control.Lens hiding (to, from)
import Control.Monad.IO.Class
import Data.Data
import Data.Foldable
import Data.Functor.Rep hiding (Rep)
import Data.Int
import Data.Traversable
import Data.Word
import Foreign.Storable
import Foreign.Ptr
import GHC.Generics
import Linear
import Linear.V
import Prelude hiding (id,(.),sequence)
import Quine.GL.Types

newtype Offset a b = Offset Int
  deriving (Data,Typeable,Generic,Eq,Ord,Show,Read)

instance Category Offset where
  id = Offset 0
  Offset a . Offset b = Offset (a + b)

newtype STD140 a = STD140 { getSTD140 :: a }
  deriving (Data,Typeable,Generic,Functor,Foldable,Traversable,Eq,Ord,Show,Read)

instance Block a => Storable (STD140 a) where
  alignment _ = alignment140 (Proxy :: Proxy a)
  sizeOf _ = sizeOf140 (Proxy :: Proxy a)
  peekByteOff p o = STD140 <$> read140 p (Offset o)
  pokeByteOff p o = write140 p (Offset o) . getSTD140

newtype STD430 a = STD430 { getSTD430 :: a }
  deriving (Data,Typeable,Generic,Functor,Foldable,Traversable,Eq,Ord,Show,Read)

instance Block a => Storable (STD430 a) where
  alignment _ = alignment430 (Proxy :: Proxy a)
  sizeOf _ = sizeOf430 (Proxy :: Proxy a)
  peekByteOff p o = STD430 <$> read430 p (Offset o)
  pokeByteOff p o = write430 p (Offset o) . getSTD430

-- | This describes how to load and store primitives
-- through a uniform/shader storage blocks according to
-- OpenGL STD140 and STD430.
--
-- There are lots of fiddly little constants around, beware.
class Block b where
  -- | As per 'Storable' 'alignment', but matching OpenGL STD140.
  alignment140 :: p b -> Int
  default alignment140 :: (Generic b, GBlock (Rep b)) => p b -> Int
  alignment140 _ = galignment140 (Proxy :: Proxy (Rep b))

  -- | As per 'Storable' 'sizeOf', but matching OpenGL STD140.
  sizeOf140 :: p b -> Int
  default sizeOf140 :: (Generic b, GBlock (Rep b)) => p b -> Int
  sizeOf140 _ = gsizeOf140 (Proxy :: Proxy (Rep b))

  -- | Structures get smashed up to a minimum of a vec4 alignment in 140 mode
  isStruct :: p b -> Bool
  isStruct _ = True

  read140 :: MonadIO m => Ptr a -> Offset a b -> m b
  default read140 :: (MonadIO m, Generic b, GBlock (Rep b)) => Ptr a -> Offset a b -> m b
  read140 p (Offset o) = liftIO $ to <$> gread140 p o

  write140 :: MonadIO m => Ptr a -> Offset a b -> b -> m ()
  default write140 :: (MonadIO m, Generic b, GBlock (Rep b)) => Ptr a -> Offset a b -> b -> m ()
  write140 p (Offset o) b = liftIO $ gwrite140 p o (from b)

  -- | As per 'Storable' 'alignment', but matching OpenGL STD430.
  alignment430 :: p b -> Int
  default alignment430 :: (Generic b, GBlock (Rep b)) => p b -> Int
  alignment430 _ = galignment430 (Proxy :: Proxy (Rep b))

  -- | As per 'Storable' 'sizeOf', but matching OpenGL STD430.
  sizeOf430 :: p b -> Int
  default sizeOf430 :: (Generic b, GBlock (Rep b)) => p b -> Int
  sizeOf430 _ = gsizeOf430 (Proxy :: Proxy (Rep b))

  read430 :: MonadIO m => Ptr a -> Offset a b -> m b
  default read430 :: (MonadIO m, Generic b, GBlock (Rep b)) => Ptr a -> Offset a b -> m b
  read430 p (Offset o) = liftIO $ to <$> gread430 p o

  write430 :: MonadIO m => Ptr a -> Offset a b -> b -> m ()
  default write430 :: (MonadIO m, Generic b, GBlock (Rep b)) => Ptr a -> Offset a b -> b -> m ()
  write430 p (Offset o) b = liftIO $ gwrite430 p o (from b)

-- | Automatically derive STD140 and STD430 alignment using GHC Generics
class GBlock f where
  galignment140 :: p f -> Int
  galignment430 :: p f -> Int
  gsizeOf140    :: p f -> Int
  gsizeOf430    :: p f -> Int
  gread140  :: Ptr a -> Int -> IO (f b)
  gread430  :: Ptr a -> Int -> IO (f b)
  gwrite140 :: Ptr a -> Int -> f b -> IO ()
  gwrite430 :: Ptr a -> Int -> f b -> IO ()

instance GBlock U1 where
  galignment140 _ = 1
  gsizeOf140    _ = 0
  galignment430 _ = 1
  gsizeOf430    _ = 0
  gread140 _ _ = return U1
  gread430 _ _ = return U1
  gwrite140 _ _ U1 = return ()
  gwrite430 _ _ U1 = return ()

instance (GBlock f, GBlock g) => GBlock (f :*: g) where
  galignment140 _ = galignment140 (Proxy :: Proxy f)
              `max` galignment140 (Proxy :: Proxy g)
  galignment430 _ = galignment430 (Proxy :: Proxy f)
              `max` galignment430 (Proxy :: Proxy g)
  gsizeOf140 _ = roundUp (gsizeOf140 (Proxy :: Proxy f)) (galignment140 (Proxy :: Proxy g)) + gsizeOf140 (Proxy :: Proxy g)
  gsizeOf430 _ = roundUp (gsizeOf430 (Proxy :: Proxy f)) (galignment430 (Proxy :: Proxy g)) + gsizeOf430 (Proxy :: Proxy g)
  gread140 p o = (:*:) <$> gread140 p o <*> gread140 p (o + roundUp (gsizeOf140 (Proxy :: Proxy f)) (galignment140 (Proxy :: Proxy g)))
  gread430 p o = (:*:) <$> gread430 p o <*> gread430 p (o + roundUp (gsizeOf430 (Proxy :: Proxy f)) (galignment430 (Proxy :: Proxy g)))
  gwrite140 p o (a :*: b) = do
    gwrite140 p o a
    gwrite140 p (o + roundUp (gsizeOf140 (Proxy :: Proxy f)) (galignment140 (Proxy :: Proxy g))) b
  gwrite430 p o (a :*: b) = do
    gwrite430 p o a
    gwrite430 p (o + roundUp (gsizeOf430 (Proxy :: Proxy f)) (galignment430 (Proxy :: Proxy g))) b

instance GBlock f => GBlock (M1 S c f) where
  galignment140 _ = galignment140 (Proxy :: Proxy f)
  galignment430 _ = galignment430 (Proxy :: Proxy f)
  gsizeOf140    _ = gsizeOf140 (Proxy :: Proxy f)
  gsizeOf430    _ = gsizeOf430 (Proxy :: Proxy f)
  gread140 p o = M1 <$> gread140 p o
  gread430 p o = M1 <$> gread430 p o
  gwrite140 p o (M1 a) = gwrite140 p o a
  gwrite430 p o (M1 a) = gwrite430 p o a

instance GBlock f => GBlock (M1 C c f) where
  galignment140 _ = lcm 16 $ galignment140 (Proxy :: Proxy f)
  galignment430 _ = galignment430 (Proxy :: Proxy f)
  gsizeOf140    _ = gsizeOf140 (Proxy :: Proxy f)
  gsizeOf430    _ = gsizeOf430 (Proxy :: Proxy f)
  gread140 p o = M1 <$> gread140 p o
  gread430 p o = M1 <$> gread430 p o
  gwrite140 p o (M1 a) = gwrite140 p o a
  gwrite430 p o (M1 a) = gwrite430 p o a

instance GBlock f => GBlock (M1 D c f) where
  galignment140 _ = galignment140 (Proxy :: Proxy f)
  galignment430 _ = galignment430 (Proxy :: Proxy f)
  gsizeOf140    _ = gsizeOf140 (Proxy :: Proxy f)
  gsizeOf430    _ = gsizeOf430 (Proxy :: Proxy f)
  gread140 p o = M1 <$> gread140 p o
  gread430 p o = M1 <$> gread430 p o
  gwrite140 p o (M1 a) = gwrite140 p o a
  gwrite430 p o (M1 a) = gwrite430 p o a

instance Block c => GBlock (K1 i c) where
  galignment140 _ = alignment140 (Proxy :: Proxy c)
  galignment430 _ = alignment430 (Proxy :: Proxy c)
  gsizeOf140    _ = sizeOf140 (Proxy :: Proxy c)
  gsizeOf430    _ = sizeOf430 (Proxy :: Proxy c)
  gread140 p o = K1 <$> read140 p (Offset o)
  gread430 p o = K1 <$> read430 p (Offset o)
  gwrite140 p o (K1 a) = write140 p (Offset o) a
  gwrite430 p o (K1 a) = write430 p (Offset o) a

toBool :: Int32 -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> Int32
fromBool False = 0
fromBool True = 1

instance Block Bool where
  alignment140 _ = 4
  sizeOf140 _    = 4
  alignment430 _ = 4
  sizeOf430 _    = 4
  isStruct _ = False
  read140  p (Offset o)   = liftIO $ toBool <$> peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o (fromBool b)
  read430 = read140
  write430 = write140

instance Block Float where
  alignment140 _ = 4
  sizeOf140 _    = 4
  alignment430 _ = 4
  sizeOf430 _    = 4
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block Double where
  alignment140 _ = 8
  sizeOf140 _    = 8
  alignment430 _ = 8
  sizeOf430 _    = 8
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block Int32 where
  alignment140 _ = 4
  sizeOf140 _    = 4
  alignment430 _ = 4
  sizeOf430 _    = 4
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block Word32 where
  alignment140 _ = 4
  sizeOf140 _    = 4
  alignment430 _ = 4
  sizeOf430 _    = 4
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block BVec2 where
  alignment140 _ = 8
  sizeOf140 _    = 8
  alignment430 _ = 8
  sizeOf430 _    = 8
  isStruct _ = False
  read140  p (Offset o)   = liftIO $ fmap toBool <$> peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o (fmap fromBool b)
  read430 = read140
  write430 = write140

instance Block BVec3 where
  alignment140 _ = 16
  sizeOf140 _    = 12
  alignment430 _ = 16
  sizeOf430 _    = 12
  isStruct _ = False
  read140  p (Offset o)   = liftIO $ fmap toBool <$> peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o (fmap fromBool b)
  read430 = read140
  write430 = write140

instance Block BVec4 where
  alignment140 _ = 16
  sizeOf140 _    = 16
  alignment430 _ = 16
  sizeOf430 _    = 16
  isStruct _ = False
  read140  p (Offset o)   = liftIO $ fmap toBool <$> peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o (fmap fromBool b)
  read430 = read140
  write430 = write140

instance Block Vec2 where
  alignment140 _ = 8
  sizeOf140 _    = 8
  alignment430 _ = 8
  sizeOf430 _    = 8
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block Vec3 where
  alignment140 _ = 16
  sizeOf140 _    = 12
  alignment430 _ = 16
  sizeOf430 _    = 12
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block Vec4 where
  alignment140 _ = 16
  sizeOf140 _    = 16
  alignment430 _ = 16
  sizeOf430 _    = 16
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block DVec2 where
  alignment140 _ = 16
  sizeOf140 _    = 16
  alignment430 _ = 16
  sizeOf430 _    = 16
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block DVec3 where
  alignment140 _ = 32
  sizeOf140 _    = 24
  alignment430 _ = 32
  sizeOf430 _    = 24
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block DVec4 where
  alignment140 _ = 32
  sizeOf140 _    = 32
  alignment430 _ = 32
  sizeOf430 _    = 32
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block IVec2 where
  alignment140 _ = 8
  sizeOf140 _    = 8
  alignment430 _ = 8
  sizeOf430 _    = 8
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block IVec3 where
  alignment140 _ = 16
  sizeOf140 _    = 12
  alignment430 _ = 16
  sizeOf430 _    = 12
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block IVec4 where
  alignment140 _ = 16
  sizeOf140 _    = 16
  alignment430 _ = 16
  sizeOf430 _    = 16
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block UVec2 where
  alignment140 _ = 8
  sizeOf140 _    = 8
  alignment430 _ = 8
  sizeOf430 _    = 8
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block UVec3 where
  alignment140 _ = 16
  sizeOf140 _    = 12
  alignment430 _ = 16
  sizeOf430 _    = 12
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block UVec4 where
  alignment140 _ = 16
  sizeOf140 _    = 16
  alignment430 _ = 16
  sizeOf430 _    = 16
  isStruct _ = False
  read140 p (Offset o) = liftIO $ peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b
  read430 = read140
  write430 = write140

instance Block Mat2 where
  alignment140 _ = 16 -- per vec4, despite being vec2
  sizeOf140    _ = 32 -- 2 columns, each rounded up to vec4 size
  alignment430 _ = 8  -- vec2
  sizeOf430    _ = 16 -- 2 columns, each vec2 in size
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V2 a c <- peekByteOff p o
    V2 b d <- peekByteOff p (o + 16)
    return $ V2 (V2 a b) (V2 c d)
  write140 p (Offset o)
    (V2 (V2 a b)
        (V2 c d)) = liftIO $ do
    pokeByteOff p o        (V2 a b)
    pokeByteOff p (o + 16) (V2 c d)
  read430 p (Offset o) = liftIO $ do
    V2 a c <- peekByteOff p o
    V2 b d <- peekByteOff p (o + 8)
    return $ V2 (V2 a b) (V2 c d)
  write430 p (Offset o)
    (V2 (V2 a b)
        (V2 c d)) = liftIO $ do
    pokeByteOff p o       (V2 a b)
    pokeByteOff p (o + 8) (V2 c d)

instance Block DMat2 where
  alignment140 _ = 16 -- dvec2 = vec4 in size
  sizeOf140    _ = 32 -- 2 columns, perfect fit!
  alignment430 _ = 16 -- dvec2 = vec4 in size
  sizeOf430    _ = 32 -- 2 columns, perfect fit!
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V2 a c <- peekByteOff p o
    V2 b d <- peekByteOff p (o + 16)
    return $ V2 (V2 a b) (V2 c d)
  write140 p (Offset o)
    (V2 (V2 a b)
        (V2 c d)) = liftIO $ do
    pokeByteOff p o        (V2 a b)
    pokeByteOff p (o + 16) (V2 c d)
  read430 = read140
  write430 = write140

instance Block Mat3x2 where
  alignment140 _ = 16 -- per vec4, despite being vec3s
  sizeOf140    _ = 32 -- 2 columns, each rounded up to vec4 size
  alignment430 _ = 16 -- per vec4, despite being vec3s
  sizeOf430    _ = 32 -- 2 columns, each rounded up to vec4 size
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V3 a c e <- peekByteOff p o
    V3 b d f <- peekByteOff p (o + 16)
    return $ V3 (V2 a b) (V2 c d) (V2 e f)
  write140 p (Offset o)
    (V3 (V2 a b)
        (V2 c d)
        (V2 e f)) = liftIO $ do
    pokeByteOff p o        (V3 a b e)
    pokeByteOff p (o + 16) (V3 c d f)
  read430 = read140
  write430 = write140

instance Block DMat3x2 where
  alignment140 _ = 32 -- dvec3s = 24, rounded up to 2*vec4 in size
  sizeOf140    _ = 64 -- 2 columns, each rounded up to 2*vec4 size
  alignment430 _ = 32 -- dvec3s = 24, rounded up to 2*vec4 in size
  sizeOf430    _ = 64 -- 2 columns, each rounded up to 2*vec4 size
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V3 a c e <- peekByteOff p o
    V3 b d f <- peekByteOff p (o + 32)
    return $ V3 (V2 a b) (V2 c d) (V2 e f)
  write140 p (Offset o)
    (V3 (V2 a b)
        (V2 c d)
        (V2 e f)) = liftIO $ do
    pokeByteOff p o        (V3 a b e)
    pokeByteOff p (o + 32) (V3 c d f)
  read430 = read140
  write430 = write140

instance Block Mat4x2 where
  alignment140 _ = 16 -- per vec4
  sizeOf140    _ = 32 -- 2 columns
  alignment430 _ = 16 -- per vec4
  sizeOf430    _ = 32 -- 2 columns
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V4 a c e g <- peekByteOff p o
    V4 b d f h <- peekByteOff p (o + 16)
    return $ V4 (V2 a b) (V2 c d) (V2 e f) (V2 g h)
  write140 p (Offset o)
    (V4 (V2 a b)
        (V2 c d)
        (V2 e f)
        (V2 g h)) = liftIO $ do
    pokeByteOff p o        (V4 a b e g)
    pokeByteOff p (o + 16) (V4 c d f h)
  read430 = read140
  write430 = write140

instance Block DMat4x2 where
  alignment140 _ = 32 -- per dvec4 = 2*vec4
  sizeOf140    _ = 64 -- 2 columns
  alignment430 _ = 32 -- per dvec4 = 2*vec4
  sizeOf430    _ = 64 -- 2 columns
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V4 a c e g <- peekByteOff p o
    V4 b d f h <- peekByteOff p (o + 32)
    return $ V4 (V2 a b) (V2 c d) (V2 e f) (V2 g h)
  write140 p (Offset o)
    (V4 (V2 a b)
        (V2 c d)
        (V2 e f)
        (V2 g h)) = liftIO $ do
    pokeByteOff p o        (V4 a b e g)
    pokeByteOff p (o + 32) (V4 c d f h)
  read430 = read140
  write430 = write140

instance Block Mat2x3 where
  alignment140 _ = 16 -- per vec4, despite being vec2 by array rules
  sizeOf140    _ = 48 -- 3 columns, each a vec4 in size, despite being vec2s
  alignment430 _ = 8  -- per vec2
  sizeOf430    _ = 24 -- 3 columns, each a vec2 in size
  isStruct  _ = False
  read140 p (Offset o) = liftIO $ do
    V2 a d <- peekByteOff p o
    V2 b e <- peekByteOff p (o + 16)
    V2 c f <- peekByteOff p (o + 32)
    return $ V2 (V3 a b c)
                (V3 d e f)
  write140 p (Offset o)
    (V2 (V3 a b c)
        (V3 d e f)) = liftIO $ do
    pokeByteOff p o        (V2 a d)
    pokeByteOff p (o + 16) (V2 b e)
    pokeByteOff p (o + 32) (V2 c f)
  read430 p (Offset o) = liftIO $ do
    V2 a d <- peekByteOff p o
    V2 b e <- peekByteOff p (o + 8)
    V2 c f <- peekByteOff p (o + 16)
    return $ V2 (V3 a b c)
                (V3 d e f)
  write430 p (Offset o)
    (V2 (V3 a b c)
        (V3 d e f)) = liftIO $ do
    pokeByteOff p o        (V2 a d)
    pokeByteOff p (o + 8)  (V2 b e)
    pokeByteOff p (o + 16) (V2 c f)

instance Block DMat2x3 where
  alignment140 _ = 16 -- a dvec2 is one vec4 in size
  sizeOf140    _ = 48 -- 3 columns, each a vec4 in size
  alignment430 _ = 16 -- a dvec2 is one vec4 in size
  sizeOf430    _ = 48 -- 3 columns, each a vec4 in size
  isStruct     _ = False
  read140 p (Offset o) = liftIO $ do
    V2 a d <- peekByteOff p o
    V2 b e <- peekByteOff p (o + 16)
    V2 c f <- peekByteOff p (o + 32)
    return $ V2 (V3 a b c)
                (V3 d e f)
  write140 p (Offset o)
    (V2 (V3 a b c)
        (V3 d e f)) = liftIO $ do
    pokeByteOff p o        (V2 a d)
    pokeByteOff p (o + 16) (V2 b e)
    pokeByteOff p (o + 32) (V2 c f)
  read430 = read140
  write430 = write140

instance Block Mat3 where
  alignment140 _ = 16 -- per vec4, despite being vec3
  sizeOf140    _ = 48 -- 3 columns, each rounded up to vec4 size
  alignment430 _ = 16 -- a dvec2 is one vec4 in size
  sizeOf430    _ = 48 -- 3 columns, each a vec4 in size
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V3 a d g <- peekByteOff p o
    V3 b e h <- peekByteOff p (o + 16)
    V3 c f i <- peekByteOff p (o + 24)
    return $ V3 (V3 a b c)
                (V3 d e f)
                (V3 g h i)
  write140 p (Offset o)
    (V3 (V3 a b c)
        (V3 d e f)
        (V3 g h i)) = liftIO $ do
    pokeByteOff p o        (V3 a d g)
    pokeByteOff p (o + 16) (V3 b e h)
    pokeByteOff p (o + 32) (V3 c f i)
  read430 = read140
  write430 = write140

instance Block DMat3 where
  alignment140 _ = 32 -- dvec3 = 1.5vec4, round up to 2*vec2
  sizeOf140    _ = 96 -- 3 columns, each rounded up to 2*vec4 size
  alignment430 _ = 32 -- a dvec2 is one vec4 in size
  sizeOf430    _ = 96 -- 3 columns, each a vec4 in size
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V3 a d g <- peekByteOff p o
    V3 b e h <- peekByteOff p (o + 32)
    V3 c f i <- peekByteOff p (o + 64)
    return $ V3 (V3 a b c)
                (V3 d e f)
                (V3 g h i)
  write140 p (Offset o)
    (V3 (V3 a b c)
        (V3 d e f)
        (V3 g h i)) = liftIO $ do
    pokeByteOff p o        (V3 a d g)
    pokeByteOff p (o + 32) (V3 b e h)
    pokeByteOff p (o + 64) (V3 c f i)
  read430 = read140
  write430 = write140

instance Block Mat4x3 where
  alignment140 _ = 16 -- per vec4
  sizeOf140    _ = 48 -- 3 columns
  alignment430 _ = 16 -- per vec4
  sizeOf430    _ = 48 -- 3 columns
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V4 a d g j <- peekByteOff p o
    V4 b e h k <- peekByteOff p (o + 16)
    V4 c f i l <- peekByteOff p (o + 32)
    return $ V4 (V3 a b c)
                (V3 d e f)
                (V3 g h i)
                (V3 j k l)
  write140 p (Offset o)
    (V4 (V3 a b c)
        (V3 d e f)
        (V3 g h i)
        (V3 j k l)) = liftIO $ do
    pokeByteOff p o        (V4 a d g j)
    pokeByteOff p (o + 16) (V4 b e h k)
    pokeByteOff p (o + 32) (V4 c f i l)
  read430 = read140
  write430 = write140

instance Block DMat4x3 where
  alignment140 _ = 32 -- dvec4 is 2*vec4 in size
  sizeOf140    _ = 48 -- 3 columns
  alignment430 _ = 32 -- dvec4 is 2*vec4 in size
  sizeOf430    _ = 48 -- 3 columns
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V4 a d g j <- peekByteOff p o
    V4 b e h k <- peekByteOff p (o + 32)
    V4 c f i l <- peekByteOff p (o + 64)
    return $ V4 (V3 a b c)
                (V3 d e f)
                (V3 g h i)
                (V3 j k l)
  write140 p (Offset o)
    (V4 (V3 a b c)
        (V3 d e f)
        (V3 g h i)
        (V3 j k l)) = liftIO $ do
    pokeByteOff p o        (V4 a d g j)
    pokeByteOff p (o + 32) (V4 b e h k)
    pokeByteOff p (o + 64) (V4 c f i l)
  read430 = read140
  write430 = write140

instance Block Mat2x4 where
  alignment140 _ = 16 -- 2 rows, but vec2 rounds up to vec4 as per array
  sizeOf140    _ = 64 -- 4 columns, each rounded up to 16 bytes each
  alignment430 _ = 8  -- 2 rows, each a vec2 in size
  sizeOf430    _ = 32 -- 4 columns
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V2 a e <- peekByteOff p o
    V2 b f <- peekByteOff p (o + 16)
    V2 c g <- peekByteOff p (o + 32)
    V2 d h <- peekByteOff p (o + 48)
    return $ V2 (V4 a b c d)
                (V4 e f g h)
  write140 p (Offset o)
    (V2 (V4 a b c d)
        (V4 e f g h)) = liftIO $ do
    pokeByteOff p o        (V2 a e)
    pokeByteOff p (o + 16) (V2 b f)
    pokeByteOff p (o + 32) (V2 c g)
    pokeByteOff p (o + 48) (V2 d h)
  read430 p (Offset o) = liftIO $ do
    V2 a e <- peekByteOff p o
    V2 b f <- peekByteOff p (o + 8)
    V2 c g <- peekByteOff p (o + 16)
    V2 d h <- peekByteOff p (o + 24)
    return $ V2 (V4 a b c d)
                (V4 e f g h)
  write430 p (Offset o)
    (V2 (V4 a b c d)
        (V4 e f g h)) = liftIO $ do
    pokeByteOff p o        (V2 a e)
    pokeByteOff p (o + 8) (V2 b f)
    pokeByteOff p (o + 16) (V2 c g)
    pokeByteOff p (o + 24) (V2 d h)

instance Block DMat2x4 where
  alignment140 _ = 16 -- 2 rows, and dvec2 = vec4 in size
  sizeOf140    _ = 64 -- 4 columns, perfect fit
  alignment430 _ = 16 -- 2 rows, and dvec2 = vec4 in size
  sizeOf430    _ = 64 -- 4 columns, perfect fit
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V2 a e <- peekByteOff p o
    V2 b f <- peekByteOff p (o + 16)
    V2 c g <- peekByteOff p (o + 32)
    V2 d h <- peekByteOff p (o + 48)
    return $ V2 (V4 a b c d)
                (V4 e f g h)
  write140 p (Offset o)
    (V2 (V4 a b c d)
        (V4 e f g h)) = liftIO $ do
    pokeByteOff p o        (V2 a e)
    pokeByteOff p (o + 16) (V2 b f)
    pokeByteOff p (o + 32) (V2 c g)
    pokeByteOff p (o + 48) (V2 d h)
  read430 = read140
  write430 = write140

instance Block Mat3x4 where
  alignment140 _ = 16 -- 3 rows, but vec3 rounds up to vec4
  sizeOf140    _ = 64 -- 4 columns, each rounded up to 16 bytes each
  alignment430 _ = 16 -- 3 rows, but vec3 rounds up to vec4
  sizeOf430    _ = 64 -- 4 columns, each rounded up to 16 bytes each
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V3 a e i <- peekByteOff p o
    V3 b f j <- peekByteOff p (o + 16)
    V3 c g k <- peekByteOff p (o + 32)
    V3 d h l <- peekByteOff p (o + 48)
    return $ V3 (V4 a b c d)
                (V4 e f g h)
                (V4 i j k l)
  write140 p (Offset o)
    (V3 (V4 a b c d)
        (V4 e f g h)
        (V4 i j k l)) = liftIO $ do
    pokeByteOff p o        (V3 a e i)
    pokeByteOff p (o + 16) (V3 b f j)
    pokeByteOff p (o + 32) (V3 c g k)
    pokeByteOff p (o + 48) (V3 d h l)
  read430 = read140
  write430 = write140

instance Block DMat3x4 where
  alignment140 _ = 32  -- 3 rows, but dvec3 rounds up to 2*vec4
  sizeOf140    _ = 128 -- 4 columns, each rounded up to 32 bytes each
  alignment430 _ = 32  -- 3 rows, but dvec3 rounds up to 2*vec4
  sizeOf430    _ = 128 -- 4 columns, each rounded up to 32 bytes each
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V3 a e i <- peekByteOff p o
    V3 b f j <- peekByteOff p (o + 32)
    V3 c g k <- peekByteOff p (o + 64)
    V3 d h l <- peekByteOff p (o + 96)
    return $ V3 (V4 a b c d)
                (V4 e f g h)
                (V4 i j k l)
  write140 p (Offset o)
    (V3 (V4 a b c d)
        (V4 e f g h)
        (V4 i j k l)) = liftIO $ do
    pokeByteOff p o        (V3 a e i)
    pokeByteOff p (o + 32) (V3 b f j)
    pokeByteOff p (o + 64) (V3 c g k)
    pokeByteOff p (o + 96) (V3 d h l)
  read430 = read140
  write430 = write140

instance Block Mat4 where
  alignment140 _ = 16
  sizeOf140    _ = 64
  alignment430 _ = 16
  sizeOf430    _ = 64
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V4 a e i m <- peekByteOff p o
    V4 b f j n <- peekByteOff p (o + 16)
    V4 c g k q <- peekByteOff p (o + 32)
    V4 d h l r <- peekByteOff p (o + 48)
    return $ V4 (V4 a b c d)
                (V4 e f g h)
                (V4 i j k l)
                (V4 m n q r)
  write140 p (Offset o)
    (V4 (V4 a b c d)
        (V4 e f g h)
        (V4 i j k l)
        (V4 m n q r)) = liftIO $ do
    pokeByteOff p o        (V4 a e i m)
    pokeByteOff p (o + 16) (V4 b f j n)
    pokeByteOff p (o + 32) (V4 c g k q)
    pokeByteOff p (o + 48) (V4 d h l r)
  read430 = read140
  write430 = write140

instance Block DMat4 where
  alignment140 _ = 32
  sizeOf140    _ = 128
  alignment430 _ = 32
  sizeOf430    _ = 128
  isStruct _ = False
  read140 p (Offset o) = liftIO $ do
    V4 a e i m <- peekByteOff p o
    V4 b f j n <- peekByteOff p (o + 32)
    V4 c g k q <- peekByteOff p (o + 64)
    V4 d h l r <- peekByteOff p (o + 96)
    return $ V4 (V4 a b c d)
                (V4 e f g h)
                (V4 i j k l)
                (V4 m n q r)
  write140 p (Offset o)
    (V4 (V4 a b c d)
        (V4 e f g h)
        (V4 i j k l)
        (V4 m n q r)) = liftIO $ do
    pokeByteOff p o        (V4 a e i m)
    pokeByteOff p (o + 32) (V4 b f j n)
    pokeByteOff p (o + 64) (V4 c g k q)
    pokeByteOff p (o + 96) (V4 d h l r)
  read430 = read140
  write430 = write140

instance (Dim n, Block a) => Block (V n a) where
  isStruct _ = isStruct (Proxy :: Proxy a)
  alignment140 _
    | isStruct (Proxy :: Proxy a) = lcm 16 n
    | otherwise = n
    where n = alignment140 (Proxy :: Proxy a)
  alignment430 _ = alignment430 (Proxy :: Proxy a)
  sizeOf140 _ = roundUp (sizeOf140 (Proxy :: Proxy a)) (alignment140 (Proxy :: Proxy a)) * fromIntegral (reflectDim (Proxy :: Proxy n))
  sizeOf430 _ = roundUp (sizeOf430 (Proxy :: Proxy a)) (alignment430 (Proxy :: Proxy a)) * fromIntegral (reflectDim (Proxy :: Proxy n))
  read140 p (Offset o) = liftIO $ sequence $ tabulate $ \i -> read140 p $ Offset (o + i*d) where
    d = roundUp (sizeOf140 (Proxy :: Proxy a)) (alignment140 (Proxy :: Proxy a))
  write140 p (Offset o) v = liftIO $ iforM_ v $ \i -> write140 p (Offset (o + i*d)) where
    d = roundUp (sizeOf140 (Proxy :: Proxy a)) (alignment140 (Proxy :: Proxy a))
  read430 p (Offset o) = liftIO $ sequence $ tabulate $ \i -> read430 p $ Offset (o + i*d) where
    d = roundUp (sizeOf430 (Proxy :: Proxy a)) (alignment430 (Proxy :: Proxy a))
  write430 p (Offset o) v = liftIO $ iforM_ v $ \i -> write430 p (Offset (o + i*d)) where
    d = roundUp (sizeOf430 (Proxy :: Proxy a)) (alignment430 (Proxy :: Proxy a))

-- | @roundUp k n@ rounds up k up to an integral multiple of n
roundUp :: Int -> Int -> Int
roundUp k n = k + mod (n - k) n

instance (Block a, Block b) => Block (a,b)
instance (Block a, Block b, Block c) => Block (a,b,c)
