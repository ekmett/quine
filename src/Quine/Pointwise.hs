{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Quine.Pointwise
  ( Pointwise(..)
  , _Pointwise
  , liftP1, liftP2, liftP3
  , smoothstep, smoothstep1
  , clamp, clamp1
  , mix, mix1
  , step, step1
  ) where

import Control.Applicative
import Control.Lens
import Data.Complex
import Data.Functor.Rep
import Data.Int
import Data.Word
import Linear
import Linear.Plucker
import Numeric.Half

type V s = F s (An s)

class (Monad (F s), Applicative (F s), Additive (F s)) => Pointwise s where
  type F  s :: * -> *
  type An s :: *

  toV   :: s -> V s
  default toV :: (F s ~ V1, s ~ An s) => s -> V s
  toV a = V1 a

  fromV :: V s -> s
  default fromV :: (F s ~ V1, s ~ An s) => V s -> s
  fromV (V1 a) = a

_Pointwise :: (Pointwise s, Pointwise t) => Iso s t (V s) (V t)
_Pointwise = iso toV fromV

instance Pointwise Half where type F Half = V1; type An Half = Half
instance Pointwise Float where type F Float = V1; type An Float = Float
instance Pointwise Double where type F Double = V1; type An Double = Double
instance Pointwise Int where type F Int = V1; type An Int = Int
instance Pointwise Int8 where type F Int8 = V1; type An Int8 = Int8
instance Pointwise Int16 where type F Int16 = V1; type An Int16 = Int16
instance Pointwise Int32 where type F Int32 = V1; type An Int32 = Int32
instance Pointwise Int64 where type F Int64 = V1; type An Int64 = Int64
instance Pointwise Word where type F Word = V1; type An Word = Word
instance Pointwise Word8 where type F Word8 = V1; type An Word8 = Word8
instance Pointwise Word16 where type F Word16 = V1; type An Word16 = Word16
instance Pointwise Word32 where type F Word32 = V1; type An Word32 = Word32
instance Pointwise Word64 where type F Word64 = V1; type An Word64 = Word64
instance Pointwise (V3 a) where type F (V3 a) = V3; type An (V3 a) = a; toV = id; fromV = id
instance Pointwise (V2 a) where type F (V2 a) = V2; type An (V2 a) = a; toV = id; fromV = id
instance Pointwise (V1 a) where type F (V1 a) = V1; type An (V1 a) = a; toV = id; fromV = id
instance Pointwise (V0 a) where type F (V0 a) = V0; type An (V0 a) = a; toV = id; fromV = id
instance Pointwise (Plucker a) where type F (Plucker a) = Plucker; type An (Plucker a) = a; toV = id; fromV = id
instance Pointwise (Quaternion a) where type F (Quaternion a) = Quaternion; type An (Quaternion a) = a; toV = id; fromV = id
instance Pointwise (Complex a) where type F (Complex a) = Complex; type An (Complex a) = a; toV = id; fromV = id

liftP1 :: (Pointwise s, Pointwise t, F s ~ F t) => (An s -> An t) -> s -> t
liftP1 f a = fromV $ fmap f (toV a)

liftP2 :: (Pointwise s, Pointwise t, Pointwise u, F s ~ F t, F t ~ F u) => (An s -> An t -> An u) -> s -> t -> u
liftP2 f a b = fromV $ liftA2 f (toV a) (toV b)

liftP3 :: (Pointwise s, Pointwise t, Pointwise u, Pointwise v, F s ~ F t, F t ~ F u, F u ~ F v) => (An s -> An t -> An u -> An v) -> s -> t -> u -> v
liftP3 f a b c = fromV $ liftA3 f (toV a) (toV b) (toV c)

clamp :: (Pointwise s, Ord (An s)) => s -> s -> s -> s
clamp = liftP3 clamp1 where 

clamp1 :: Ord a => a -> a -> a -> a
clamp1 x l h = min (max x l) h

smoothstep :: (Pointwise s, Fractional (An s), Ord (An s)) => s -> s -> s -> s
smoothstep = liftP3 smoothstep1

smoothstep1 :: (Fractional a, Ord a) => a -> a -> a -> a
smoothstep1 l h x = t*t*(3-2*t) where
  t = clamp1 ((x-l)/(h-l)) 0 1

mix :: (Pointwise s, Num (An s)) => s -> s -> s -> s
mix = liftP3 mix1

mix1 :: Num a => a -> a -> a -> a
mix1 x y a = x*(1-a)+y*a

step :: (Pointwise s, Num (An s), Ord (An s)) => s -> s -> s
step = liftP2 step1

step1 :: (Num a, Ord a) => a -> a -> a 
step1 edge x 
  | x < edge  = 0
  | otherwise = 1
