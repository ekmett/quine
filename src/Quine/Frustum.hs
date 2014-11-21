{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Quine.Frustum
  ( Frustum(..)
  , OverlapsFrustum(..)
 -- , buildFrustum
  ) where

import Control.Lens
import Data.Data
import Data.Vector -- .Storable
import GHC.Generics
import Linear
import Prelude hiding (any, all)
import Quine.Bounding.Box
import Quine.GL.Types
import Quine.Plane

data Frustum = Frustum { planes :: Vector Plane, points :: Vector Vec3 }
  deriving (Show,Eq,Ord,Generic,Typeable,Data)

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
  overlapsBox (Frustum ps qs) b@(Box l h)
    = all (\p -> signedDistance p (pVertex p b) >= 0) ps
   && any (\q -> q^._x <= h^._x) qs
   && any (\q -> q^._y <= h^._y) qs
   && any (\q -> q^._z <= h^._z) qs
   && any (\q -> q^._x >= l^._x) qs
   && any (\q -> q^._y >= l^._y) qs
   && any (\q -> q^._z >= l^._z) qs

class OverlapsFrustum a where
  overlapsFrustum :: a -> Frustum -> Bool

instance OverlapsFrustum Box where
  overlapsFrustum = flip overlapsBox
