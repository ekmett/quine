module Quine.Color
  ( srgb
  , linear
  ) where

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
