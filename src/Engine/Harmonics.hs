{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Spherical Harmonics form a vector space.
--
-- The fast second order harmonic rotation here is based on code by John Hable's article on
-- <http://www.filmicworlds.com/2014/07/02/simple-and-fast-spherical-harmonic-rotation/ "Simple and Fast Spherical Harmonic Rotation">.
-----------------------------------------------------------------------------
module Engine.Harmonics where

import Control.Applicative
import Control.Lens
import Data.Data
import Data.Distributive
import Data.Foldable
import GHC.Generics
import Linear

class Traversable t => Harmonic t where
  rot :: Floating a => M33 a -> t a -> t a
  fromNormal :: Floating a => V3 a -> t a

newtype Band0 a = Band0 a
  deriving (Eq,Ord,Read,Show,Functor,Foldable,Traversable,Generic,Generic1,Data,Typeable)

class HasBand0 t where
  band0 :: Lens' (t a) (Band0 a)

  sh00 :: Lens' (t a) a
  sh00 = band0.sh00

instance Harmonic Band0 where
  rot _ a = a
  {-# INLINE rot #-}
  fromNormal _ = Band0 $ recip (2 * sqrt pi)
  {-# INLINE fromNormal #-}

instance Applicative Band0 where
  pure = Band0
  Band0 f <*> Band0 a = Band0 (f a)

instance Additive Band0

instance Monad Band0 where
  return = Band0
  Band0 a >>= f = f a

instance HasBand0 Band0 where
  band0 = id
  sh00 f (Band0 a) = Band0 <$> f a

instance Distributive Band0 where
  distribute f = Band0 (view sh00 <$> f)

data Band1 a = Band1 !a !a !a
  deriving (Eq,Ord,Read,Show,Functor,Foldable,Traversable,Generic,Generic1,Data,Typeable)

class HasBand1 t where
  band1            :: Lens' (t a) (Band1 a)

  sh10, sh11, sh12 :: Lens' (t a) a
  sh10 = band1.sh10
  sh11 = band1.sh11
  sh12 = band1.sh12

instance HasBand1 Band1 where
  band1 = id
  sh10 f (Band1 a b c) = f a <&> \a' -> Band1 a' b c
  sh11 f (Band1 a b c) = f b <&> \b' -> Band1 a b' c
  sh12 f (Band1 a b c) = f c <&> \c' -> Band1 a b c'

instance Applicative Band1 where
  pure a = Band1 a a a
  Band1 fa fb fc <*> Band1 xa xb xc = Band1 (fa xa) (fb xb) (fc xc)

instance Additive Band1

instance Monad Band1 where
  return a = Band1 a a a
  Band1 a b c >>= f = Band1 a' b' c' where
    Band1 a' _ _ = f a
    Band1 _ b' _ = f b
    Band1 _ _ c' = f c

instance Harmonic Band1 where
  rot (V3 (V3 m00 m01 m02)
          (V3 m10 m11 m12)
          (V3 m20 m21 m22)) (Band1 a b c) = Band1
    ( m11*a - m12*b + m10*c)
    (-m21*a + m22*b - m20*c)
    ( m01*a - m02*b + m00*c)
  {-# INLINE rot #-}
  fromNormal (V3 nx ny nz) = Band1 (negate $ k * ny) (k * nz) (negate $ k * nx) where
    k = 0.5 * sqrt 3 / sqrt pi
  {-# INLINE fromNormal #-}

instance Distributive Band1 where
  distribute f = Band1 (view sh10 <$> f) (view sh11 <$> f) (view sh12 <$> f)

data Band2 a = Band2 !a !a !a !a !a
  deriving (Eq,Ord,Read,Show,Functor,Foldable,Traversable,Generic,Generic1,Data,Typeable)

class HasBand2 t where
  band2 :: Lens' (t a) (Band2 a)

  sh20, sh21, sh22, sh23, sh24 :: Lens' (t a) a
  sh20 = band2.sh20
  sh21 = band2.sh21
  sh22 = band2.sh22
  sh23 = band2.sh23
  sh24 = band2.sh24

instance HasBand2 Band2 where
  band2 = id
  sh20 f (Band2 a b c d e) = f a <&> \a' -> Band2 a' b c d e
  sh21 f (Band2 a b c d e) = f b <&> \b' -> Band2 a b' c d e
  sh22 f (Band2 a b c d e) = f c <&> \c' -> Band2 a b c' d e
  sh23 f (Band2 a b c d e) = f d <&> \d' -> Band2 a b c d' e
  sh24 f (Band2 a b c d e) = f e <&> \e' -> Band2 a b c d e'

instance Applicative Band2 where
  pure a = Band2 a a a a a
  Band2 fa fb fc fd fe <*> Band2 xa xb xc xd xe = Band2 (fa xa) (fb xb) (fc xc) (fd xd) (fe xe)

instance Additive Band2

instance Monad Band2 where
  return a = Band2 a a a a a
  Band2 a b c d e >>= f = Band2 a' b' c' d' e' where
    Band2 a' _ _ _ _ = f a
    Band2 _ b' _ _ _ = f b
    Band2 _ _ c' _ _ = f c
    Band2 _ _ _ d' _ = f d
    Band2 _ _ _ _ e' = f e

instance Distributive Band2 where
  distribute f = Band2
    (view sh20 <$> f)
    (view sh21 <$> f)
    (view sh22 <$> f)
    (view sh23 <$> f)
    (view sh24 <$> f)

instance Harmonic Band2 where
  rot (V3 (V3 m00 m01 m02)
          (V3 m10 m11 m12)
          (V3 m20 m21 m22)) (Band2 x0 x1 x2 x3 x4) = Band2
    (sh0_x * m10 + sh1_x * m12+ sh2_x * r2y + sh3_x * r3y + sh4_x * r4y)
    (negate $ sh0_y * m20 + sh1_y * m22 + sh2_y * r2z + sh3_y * r3z + sh4_y * r4z)
    (d2 * k5)
    (negate $ sh0_x * m20 + sh1_x * m22 + sh2_x * r2z + sh3_x * r3z + sh4_x * r4z)
    (d4 * k4)
   where
    k1  = 1/3
    k2  = 2/3
    k3  = sqrt 3
    k4  = 0.5
    k5  = k3 * k4

    sh0 = x3 + x4 + x4 - x1
    sh1 = x0 + k3 * x2 + x3 + x4
    sh2 = x0
    sh3 = negate x3
    sh4 = negate x1

    r2x = m00 + m01; r2y = m10 + m11; r2z = m20 + m21
    r3x = m00 + m02; r3y = m10 + m12; r3z = m20 + m22
    r4x = m01 + m02; r4y = m11 + m12; r4z = m21 + m22

    sh0_x = sh0 * m00; sh0_y = sh0 * m10
    sh1_x = sh1 * m02; sh1_y = sh1 * m12
    sh2_x = sh2 * r2x; sh2_y = sh2 * r2y
    sh3_x = sh3 * r3x; sh3_y = sh3 * r3y
    sh4_x = sh4 * r4x; sh4_y = sh4 * r4y

    d2 = sh0 * (m20 * m20 - k1) 
       + sh1 * (m22 * m22 - k1)
       + sh2 * (r2z * r2z - k2)
       + sh3 * (r3z * r3z - k2)
       + sh4 * (r3z * r4z - k2)

    d4 = sh0_x * m00 - sh0_y * m10 
       + sh1_x * m02 - sh1_y * m12
       + sh2_x * r2x - sh2_y * r2y
       + sh3_x * r3x - sh3_y * r3y
       + sh4_x * r4x - sh4_y * r4y
  {-# INLINE rot #-}

  fromNormal (V3 nx ny nz) = Band2 
     (k2*nx*ny)
     (negate $ k2*ny*nz)
     (k3*nz*nz - k4)
     (negate $ k2*nx*nz)
     (k5*(nx*nx - ny*ny))
   where
    oosqrtpi = recip (sqrt pi)
    sqrt3 = sqrt 3
    sqrt5 = sqrt 5
    k2 = 0.5  * sqrt3 * oosqrtpi
    k3 = k2   * sqrt5
    k4 = 0.25 * sqrt5 * oosqrtpi
    k5 = 3 * k4
  {-# INLINE fromNormal #-}

data SH1 a = SH1 !(Band0 a) !(Band1 a)
  deriving (Eq,Ord,Read,Show,Functor,Foldable,Traversable,Data,Typeable,Generic,Generic1)

makeLenses ''SH1

instance HasBand0 SH1 where
  band0 f (SH1 a as) = (`SH1` as) <$> f a
  
instance HasBand1 SH1 where
  band1 f (SH1 a as) = SH1 a <$> f as

instance Applicative SH1 where
  pure a = SH1 (pure a) (pure a)
  SH1 a as <*> SH1 b bs = SH1 (a <*> b) (as <*> bs)

instance Additive SH1

instance Monad SH1 where
  return a = SH1 (return a) (return a)
  SH1 a as >>= f = SH1 b bs where
    b  = a  >>= view band0 . f
    bs = as >>= view band1 . f

instance Harmonic SH1 where
  rot r (SH1 a b) = SH1 (rot r a) (rot r b)
  {-# INLINE rot #-}
  fromNormal n = SH1 (fromNormal n) (fromNormal n)
  {-# INLINE fromNormal #-}

instance Distributive SH1 where
  distribute f = SH1
    (view band0 `collect` f)
    (view band1 `collect` f)

data SH2 a = SH2 !(Band0 a) !(Band1 a) !(Band2 a)
  deriving (Eq,Ord,Read,Show,Functor,Foldable,Traversable,Data,Typeable,Generic,Generic1)

instance HasBand0 SH2 where
  band0 f (SH2 a as ass) = (\a' -> SH2 a' as ass) <$> f a

instance HasBand1 SH2 where
  band1 f (SH2 a as ass) = (\as' -> SH2 a as' ass) <$> f as

instance HasBand2 SH2 where
  band2 f (SH2 a as ass) = SH2 a as <$> f ass

instance Applicative SH2 where
  pure a = SH2 (pure a) (pure a) (pure a)
  SH2 a as ass <*> SH2 b bs bss = SH2 (a <*> b) (as <*> bs) (ass <*> bss)

instance Additive SH2

instance Harmonic SH2 where
  rot r (SH2 a b c) = SH2 a (rot r b) (rot r c)
  {-# INLINE rot #-}
  fromNormal n = SH2 (fromNormal n) (fromNormal n) (fromNormal n)
  {-# INLINE fromNormal #-}

instance Distributive SH2 where
  distribute f = SH2
    (view band0 `collect` f)
    (view band1 `collect` f)
    (view band2 `collect` f)
