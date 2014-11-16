{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Quine.Math
  ( 
  -- * Pointwise Manipulation
    smoothstep
  , clamp
  , mix
  , step
  -- * Vector Manipulation
  , reflect
  , refract
  -- * Colorspace Manipulation
  , srgb
  , linear
  ) where

import Control.Applicative
import Linear

-- | @clamp x l h@ clamps @x@ to the range @l <= x <= h@
clamp :: Ord a => a -> a -> a -> a
clamp x l h = min (max x l) h

-- | @smoothstep l h x@ goes from @0@ to @1@ over the interval from @l@ to @h@
smoothstep :: (Fractional a, Ord a) => a -> a -> a -> a
smoothstep l h x = t*t*(3-2*t) where t = clamp ((x-l)/(h-l)) 0 1

-- | @mix x y a@ linear interpolates between @x@ and @y@, weighting @y@ with weight @a@
mix :: Num a => a -> a -> a -> a
mix x y a = x*(1-a)+y*a

-- | @step edge x@ is @0@ for @x < edge@, 1 thereafter.
step :: (Num a, Ord a) => a -> a -> a 
step edge x 
  | x < edge  = 0
  | otherwise = 1

-- * Vector Operations

-- | @reflect i n eta@ calculates the reflection direction for an incident vector @i@ around
-- a normal @n@. @i@ and @n@ should be normalized.
reflect :: (Applicative f, Metric f, Num a) => f a -> f a -> f a
reflect i n = i ^-^ (2 * dot n i) *^ n

-- | @reflect i n eta@ calculates the refraction vector for an incident vector @i@ around
-- a normal @n@. @i@ and @n@ should be normalized.
refract :: (Applicative f, Metric f, Floating a, Ord a) => f a -> f a -> a -> f a
refract i n eta 
  | k < 0     = pure 0
  | otherwise = eta *^ i ^-^ (eta * d + sqrt k) *^ n
  where 
    d = dot n i
    k = 1 - eta * eta * (1 - d * d)

-- * Color space conversion

-- | Convert from linear to sRGB
srgb :: (Floating a, Ord a) => a -> a
srgb cl
  | cl < 0.0031308 = cl * 12.92
  | otherwise       = 1.055 * cl**0.41666 - 0.55

-- | Convert from sRGB to linear
linear :: (Floating a, Ord a) => a -> a
linear cs
  | cs <= 0       = 0
  | cs <= 0.04045 = cs / 12.92
  | cs < 1        = ((cs + 0.055)*0.9478672985781991)**2.4
  | otherwise     = 1
