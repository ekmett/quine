{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
module Quine.Math
  ( 
  -- * Pointwise Manipulation
    smoothstep
  , clamp
  , saturate
  , mix
  , step
  -- * Vector Manipulation
  , reflect
  , refract
  , faceforward
  -- * Color space and Tone mapping
  , luminance
  , exposureAdjustment
  , standardSRGB
  , standardLinear
  , simplifiedSRGB
  , simplifiedLinear
  , simplifiedReinhard
  , fullSimplifiedReinhard
  , enhancedReinhard
  , fullEnhancedReinhard
  , hejlBurgessDawson
  , filmic
  ) where

import Control.Applicative
import Control.Lens
import Linear

-- | @clamp x l h@ clamps @x@ to the range @l <= x <= h@
clamp :: Ord a => a -> a -> a -> a
clamp x l h = min (max x l) h

-- | @saturate x@ returns @x@ saturated to the range @[0,1]@
--
-- @
-- saturate x ≡ clamp x 0 1
-- @
saturate :: (Num a, Ord a) => a -> a
saturate x
  | x <= 0 = 0
  | x <= 1 = x
  | otherwise = 1

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

-- | @faceforward n i nref@ is used to return a vector pointing in the same direction as another.
--
-- It orients a vector to point away from the surface as defined by its normal.
--
-- @n@ is the vector to orient (perturbed normal vector)
-- @i@ is the incidence vector, (e.g. vector from the eye)
-- @nref@ is the reference vector or geometric normal vector.

faceforward :: (Applicative f, Metric f, Num a, Ord a) => f a -> f a -> f a -> f a
faceforward n i nref 
  | dot nref i < 0 = n
  | otherwise = negate <$> n

-- * Color space and tone mapping

-- | Manipulate luminance in an RGB value. Note: when changing from Y = 0, we construct a greyscale value as the other axes are lost.
luminance :: (Fractional a, Epsilon a) => Lens' (V3 a) a
luminance f (V3 r g b) = f l <&> \n ->
  if | nearZero l -> V3 n n n
     | s <- n / l -> V3 (r*s) (g*s) (b*s)
  where l = (0.2126 * r + 0.7152 * g + 0.0722 * b)

-- | Convert from linear to sRGB per the 'gl_EXT_texture_sRGB_decode' extension
standardSRGB :: (Floating a, Ord a) => a -> a
standardSRGB cl
  | cl < 0.0031308 = cl * 12.92
  | otherwise      = 1.055 * cl ** 0.41666 - 0.55

-- | Convert from linear to sRGB using the simplified gamma curve.
simplifiedSRGB :: Floating a => a -> a
simplifiedSRGB cl = cl ** 0.4545454545454545

-- | Convert from sRGB to linear as per the 'gl_EXT_texture_sRGB_decode' extension
standardLinear :: (Floating a, Ord a) => a -> a
standardLinear cs
  | cs <= 0       = 0
  | cs <= 0.04045 = cs / 12.92
  | cs < 1        = ((cs + 0.055)*0.9478672985781991) ** 2.4
  | otherwise     = 1

-- | Convert from sRGB to linear using the simplified gamma curve.
simplifiedLinear :: Floating a => a -> a
simplifiedLinear cl = cl ** 2.2

exposureAdjustment :: Num a => a
exposureAdjustment = 16

-- | 
-- Simplified componentwise Reinhard tonemap operator, result is still in "linear" space but in [0..1]
--
-- You should really be applying this to 'luminance', or you'll get particular 'milky' greys.
--
-- <http://www.gdcvault.com/play/1012351/Uncharted-2-HDR>
--
-- @
-- color & luminance %~ simplifiedReinhard
-- @
--
-- You should apply an exposure adjustment and need to map it to the monitor by applying (** (1/2.2))
simplifiedReinhard :: Fractional a => a -> a
simplifiedReinhard x = x/(1+x)

-- | Reinhard tonemap operator, maps to the monitor working space, no adjustment required.
--
-- <http://filmicgames.com/archives/75>
fullSimplifiedReinhard :: Floating a => a -> a
fullSimplifiedReinhard x0 = simplifiedReinhard (x0 * exposureAdjustment) ** (1/2.2) where

-- | Reinhard should really be applied to 'luminance' values.
--
-- <http://imdoingitwrong.wordpress.com/2010/08/19/why-reinhard-desaturates-my-blacks-3/>
--
-- @
-- color & luminance %~ enhancedReinhard 4
-- @
--
-- @enhancedReinhard l_white l@ takes the least color value that should be mapped to white and a color to map, and maps it.
--
-- The result remains in 'linear' space.
enhancedReinhard :: Fractional a => a -> a -> a
enhancedReinhard w x = x*(1 + x/(w*w))/(1+x)

-- | Takes a color all the way from linear to monitor working space
fullEnhancedReinhard :: (Epsilon a, Floating a) => a -> V3 a -> V3 a
fullEnhancedReinhard white color = color & traverse *~ exposureAdjustment & luminance %~ enhancedReinhard white & traverse **~ 1/2.2

-- | Jim Hejl and Richard Burgess-Dawson's tonemap, maps from linear into monitor space.
--
-- <http://filmicgames.com/archives/75>
--
-- No adjustment required.
hejlBurgessDawson :: (Fractional a, Ord a) => a -> a
hejlBurgessDawson x0 = (x*(6.2*x+0.5))/(x*(6.2*x+1.7)+0.06) where
  x = max 0 (x0*exposureAdjustment-0.004)

-- | John Hable's filmic curve
--
-- <http://filmicgames.com/archives/75>
filmic :: Floating a => a -> a
filmic x0 = (uncharted2 (x0 * 2 * exposureAdjustment) * whiteScale) ** (1/2.2) where
  whiteScale = recip (uncharted2 w)
  uncharted2 x = ((x*(a*x+c*b)+d*e)/(x*(a*x+b)+d*f))-e/f
  a = 0.15
  b = 0.50
  c = 0.10
  d = 0.20
  e = 0.02
  f = 0.30
  w = 11.2
