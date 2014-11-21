{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Assumes normals are actually unit vectors
module Quine.Geometry.Plane
  ( Plane(..)
  , ToPlane(..)
  , HasPlane(..)
  , signedDistance
  , pVertex
  , nVertex
  ) where

import Control.Applicative
import Control.Lens
import Data.Data
import GHC.Generics
import Linear
import Quine.GL.Types
import Quine.Geometry.Box
import Quine.Geometry.Normal
import Quine.Geometry.Sphere

data Plane = Plane { _planeNormal :: !Vec3, _planeDistance :: !Float } deriving (Show,Ord,Eq,Typeable,Data,Generic)

instance ToNormal Plane where
  toNormal (Plane n _) = n

instance HasNormal Plane where
  normal f (Plane n d) = (`Plane` d) <$> f n

class ToNormal a => ToPlane a where
  toPlane :: a -> Plane

instance ToPlane Plane where
  toPlane = id

class (HasNormal a, ToPlane a) => HasPlane a where
  plane    :: Lens' a Plane
  distance :: Lens' a Float

instance HasPlane Plane where
  plane = id
  distance f (Plane n d) = Plane n <$> f d

signedDistance :: Plane -> Vec3 -> Float
signedDistance (Plane (V3 a b c) d) (V3 x y z) = a*x + b*y + c*z + d

-- | this is the vertex of the box in the 'most positive' position relative to this plane.
--
-- Out of all corners of the box, this will compare with the greatest signed distance
pVertex :: Plane -> Box -> Vec3
pVertex (Plane (V3 a b c) _) (Box (V3 lx ly lz) (V3 hx hy hz))
  = V3 (if a < 0 then lx else hx)
       (if b < 0 then ly else hy)
       (if c < 0 then lz else hz)
 
-- | this is the vertex of the box in the 'most negative' position relative to this plane.
--
-- Out of all corners of the box, this will compare with the least signed distance
nVertex :: Plane -> Box -> Vec3
nVertex (Plane (V3 a b c) _) (Box (V3 lx ly lz) (V3 hx hy hz))
  = V3 (if a < 0 then hx else lx)
       (if b < 0 then hy else ly)
       (if c < 0 then hz else lz)
 
instance OverlapsBox Plane where
  overlapsBox p q = signedDistance p (pVertex p q) <= 0 && signedDistance p (nVertex p q) >= 0;

class OverlapsPlane a where 
  overlapsPlane :: a -> Plane -> Bool

instance OverlapsPlane Plane where
  overlapsPlane (Plane n d) (Plane n' d')
     | n == n'   = nearZero (d - d') -- parallel, same plane?
     | n == -n'  = nearZero (d + d') -- parallel, opposite sign, same plane?
     | otherwise = True              -- non-parallel planes intersect at a line

instance OverlapsPlane Box where
  overlapsPlane = flip overlapsBox

instance OverlapsPlane Sphere where
  overlapsPlane = flip overlapsSphere

instance OverlapsSphere Plane where
  overlapsSphere p (Sphere s r) = abs (signedDistance p s) <= r
