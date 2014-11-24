{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Quine.GL.STD140
  ( Offset(..)
  , STD140(..)
  ) where

import Control.Applicative
import Control.Category
import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Foreign.Storable
import Foreign.Ptr
import Linear
import Prelude hiding (id,(.))
import Quine.GL.Types

newtype Offset a b = Offset Int

instance Category Offset where
  id = Offset 0
  Offset a . Offset b = Offset (a + b)

-- | This describes how to load and store primitives 
-- through a uniform block according to OpenGL STD140
--
-- There are lots of fiddly little constants around, beware.
class STD140 b where
  -- As per 'Storable' 'alignment', but matching OpenGL STD140.
  alignment140 :: p b -> Int
  default alignment140 :: Storable b => p b -> Int
  alignment140 _ = alignment (undefined :: b)

  -- As per 'Storable' 'alignment', but matching OpenGL STD140.
  sizeOf140 :: p b -> Int
  default sizeOf140 :: Storable b => p b -> Int
  sizeOf140 _ = sizeOf (undefined :: b)

  isStruct140 :: p b -> Bool
  isStruct140 _ = True

  read140 :: MonadIO m => Ptr a -> Offset a b -> m b
  default read140 :: (MonadIO m, Storable b) => Ptr a -> Offset a b -> m b
  read140 p (Offset o) = liftIO $ peekByteOff p o

  write140 :: MonadIO m => Ptr a -> Offset a b -> b -> m ()
  default write140 :: (MonadIO m, Storable b) => Ptr a -> Offset a b -> b -> m ()
  write140 p (Offset o) b = liftIO $ pokeByteOff p o b 

toBool :: Int32 -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> Int32
fromBool False = 0
fromBool True = 1

instance STD140 Bool where
  alignment140 _ = 4
  sizeOf140 _    = 4
  isStruct140 _ = False
  read140  p (Offset o)   = liftIO $ toBool <$> peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o (fromBool b)

instance STD140 Float where
  alignment140 _ = 4
  isStruct140 _ = False

instance STD140 Double where
  alignment140 _ = 8
  isStruct140 _ = False

instance STD140 Int32 where
  alignment140 _ = 4
  isStruct140 _ = False

instance STD140 Word32 where
  alignment140 _ = 4
  isStruct140 _ = False

instance STD140 BVec2 where
  alignment140 _ = 8
  isStruct140 _ = False
  read140  p (Offset o)   = liftIO $ fmap toBool <$> peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o (fmap fromBool b)

instance STD140 BVec3 where
  alignment140 _ = 16
  isStruct140 _ = False
  read140  p (Offset o)   = liftIO $ fmap toBool <$> peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o (fmap fromBool b)

instance STD140 BVec4 where
  alignment140 _ = 16
  isStruct140 _ = False
  read140  p (Offset o)   = liftIO $ fmap toBool <$> peekByteOff p o
  write140 p (Offset o) b = liftIO $ pokeByteOff p o (fmap fromBool b)

instance STD140 Vec2 where
  alignment140 _ = 8
  isStruct140 _ = False

instance STD140 Vec3 where
  alignment140 _ = 16
  isStruct140 _ = False

instance STD140 Vec4 where
  alignment140 _ = 16
  isStruct140 _ = False

instance STD140 DVec2 where
  alignment140 _ = 16
  isStruct140 _ = False

instance STD140 DVec3 where
  alignment140 _ = 32
  isStruct140 _ = False

instance STD140 DVec4 where
  alignment140 _ = 32
  isStruct140 _ = False

instance STD140 IVec2 where
  alignment140 _ = 8
  isStruct140 _ = False

instance STD140 IVec3 where
  alignment140 _ = 16
  isStruct140 _ = False

instance STD140 IVec4 where
  alignment140 _ = 16
  isStruct140 _ = False

instance STD140 UVec2 where
  alignment140 _ = 8
  isStruct140 _ = False

instance STD140 UVec3 where
  alignment140 _ = 16
  isStruct140 _ = False

instance STD140 UVec4 where
  alignment140 _ = 16
  isStruct140 _ = False

instance STD140 Mat2 where
  alignment140 _ = 16 -- per vec4, despite being vec2
  sizeOf140    _ = 32 -- 2 columns, each rounded up to vec4 size
  isStruct140 _ = False
  read140 p (Offset o) = liftIO $ do
    V2 a c <- peekByteOff p o
    V2 b d <- peekByteOff p (o + 16)
    return $ V2 (V2 a b) (V2 c d)
  write140 p (Offset o)
    (V2 (V2 a b)
        (V2 c d)) = liftIO $ do
    pokeByteOff p o        (V2 a b)
    pokeByteOff p (o + 16) (V2 c d)

instance STD140 DMat2 where
  alignment140 _ = 16 -- dvec2 = vec4 in size
  sizeOf140    _ = 32 -- 2 columns, perfect fit!
  isStruct140 _ = False
  read140 p (Offset o) = liftIO $ do
    V2 a c <- peekByteOff p o
    V2 b d <- peekByteOff p (o + 16)
    return $ V2 (V2 a b) (V2 c d)
  write140 p (Offset o)
    (V2 (V2 a b)
        (V2 c d)) = liftIO $ do
    pokeByteOff p o        (V2 a b)
    pokeByteOff p (o + 16) (V2 c d)

instance STD140 Mat3x2 where
  alignment140 _ = 16 -- per vec4, despite being vec3s
  sizeOf140    _ = 32 -- 2 columns, each rounded up to vec4 size
  isStruct140 _ = False
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

instance STD140 DMat3x2 where
  alignment140 _ = 32 -- dvec3s = 24, rounded up to 2*vec4 in size
  sizeOf140    _ = 64 -- 2 columns, each rounded up to 2*vec4 size
  isStruct140 _ = False
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

instance STD140 Mat4x2 where
  alignment140 _ = 16 -- per vec4
  sizeOf140    _ = 32 -- 2 columns
  isStruct140 _ = False
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

instance STD140 DMat4x2 where
  alignment140 _ = 32 -- per dvec4 = 2*vec4
  sizeOf140    _ = 64 -- 2 columns
  isStruct140 _ = False
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

instance STD140 Mat2x3 where
  alignment140 _ = 16 -- per vec4, despite being vec2 by array rules
  sizeOf140    _ = 48 -- 3 columns, each a vec4 in size, despite being vec2s
  isStruct140     _ = False
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

instance STD140 DMat2x3 where
  alignment140 _ = 16 -- a dvec2 is one vec4 in size
  sizeOf140    _ = 48 -- 3 columns, each a vec4 in size
  isStruct140     _ = False
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
  
instance STD140 Mat3 where
  alignment140 _ = 16 -- per vec4, despite being vec3
  sizeOf140    _ = 48 -- 3 columns, each rounded up to vec4 size
  isStruct140 _ = False
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

instance STD140 DMat3 where
  alignment140 _ = 32 -- dvec3 = 1.5vec4, round up to 2*vec2
  sizeOf140    _ = 96 -- 3 columns, each rounded up to 2*vec4 size
  isStruct140 _ = False
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

instance STD140 Mat4x3 where
  alignment140 _ = 16 -- per vec4
  sizeOf140    _ = 48 -- 3 columns
  isStruct140 _ = False
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

instance STD140 DMat4x3 where
  alignment140 _ = 32 -- dvec4 is 2*vec4 in size
  sizeOf140    _ = 48 -- 3 columns
  isStruct140 _ = False
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

instance STD140 Mat2x4 where
  alignment140 _ = 16 -- 2 rows, but vec2 rounds up to vec4 as per array
  sizeOf140    _ = 64 -- 4 columns, each rounded up to 16 bytes each
  isStruct140 _ = False
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

instance STD140 DMat2x4 where
  alignment140 _ = 16 -- 2 rows, and dvec2 = vec4 in size
  sizeOf140    _ = 64 -- 4 columns, perfect fit
  isStruct140 _ = False
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

instance STD140 Mat3x4 where
  alignment140 _ = 16 -- 3 rows, but vec3 rounds up to vec4
  sizeOf140    _ = 64 -- 4 columns, each rounded up to 16 bytes each
  isStruct140 _ = False
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

instance STD140 DMat3x4 where
  alignment140 _ = 32  -- 3 rows, but dvec3 rounds up to 2*vec4
  sizeOf140    _ = 128 -- 4 columns, each rounded up to 32 bytes each
  isStruct140 _ = False
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

instance STD140 Mat4 where
  alignment140 _ = 16
  sizeOf140 _ = 64
  isStruct140 _ = False
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

instance STD140 DMat4 where
  alignment140 _ = 32
  sizeOf140 _ = 128
  isStruct140 _ = False
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
