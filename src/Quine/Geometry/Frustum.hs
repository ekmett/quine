{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Quine.Geometry.Frustum
  ( Frustum(..)
  , HasFrustum(..)
  , OverlapsFrustum(..)
 -- , buildFrustum
  ) where

import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Data.Vector
import Data.Word
import GHC.Generics
import Linear hiding (frustum)
import Linear.V
import Prelude hiding (any, all)
import Quine.Geometry.Box
import Quine.Geometry.Plane
import Quine.Geometry.Sphere
import Quine.GL.Block
import Quine.GL.Types

data Frustum = Frustum { _frustumPlanes :: V 6 Plane, _frustumPoints :: V 8 Vec3 }
  deriving (Show,Eq,Ord,Generic)

makeClassy ''Frustum

-- | encoded to match @shaders/geometry/frustum.h@
instance Block Frustum

{-
-- | @buildFrustum origin direction nearZ farZ fovy aspectRatio@
buildFrustum :: Vec3 -> Vec3 -> Vec3 -> Float -> Float -> Float -> Float -> Frustum
buildFrustum origin dir up near far fovy aspect = undefined -- TODO
  where
    t = tan (fovy*0.5)
    nc = origin + near*^dir
    fc = origin + far*^dir
    nh = t * near
    fh = t * far
    nw = nh * aspect
    fw = fh * aspect
-}

instance OverlapsBox Frustum where
  overlapsBox (Frustum (V ps) (V qs)) b@(Box (V3 lx ly lz) (V3 hx hy hz))
    = all (\p -> signedDistance p (pVertex p b) >= 0) ps && foldl' (\r q -> r .|. mask q) (0::Word8) qs == 0x3f
    where mask (V3 x y z) = 0 & partsOf bits .~ [ x >= lx, y >= ly, z >= lz, x <= hx, y <= hy, z <= hz]

class OverlapsFrustum a where
  overlapsFrustum :: a -> Frustum -> Bool

instance OverlapsFrustum Box where
  overlapsFrustum = flip overlapsBox

instance OverlapsSphere Frustum where
  overlapsSphere (Frustum (V ps) _) (Sphere c r)
    = all (\p -> signedDistance p c + r >= 0) ps

instance OverlapsFrustum Sphere where
  overlapsFrustum = flip overlapsSphere
