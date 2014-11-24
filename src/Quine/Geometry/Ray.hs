{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- These cache the recipriocal direction.
--
-- <http://tavianator.com/2011/05/fast-branchless-raybounding-box-intersections/>
--------------------------------------------------------------------
module Quine.Geometry.Ray
  ( Ray(..)
  , _Ray
  , ToRay(..)
  , HasRay(..)
  , OverlapsRay(..)
  , evalRay
  ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Extras
import Data.Data
import Data.Foldable
import Data.Semigroup
import GHC.Generics
import Linear
import Quine.Geometry.Box
import Quine.Geometry.Position
import Quine.GL.Block
import Quine.GL.Types
import Quine.Instances ()

data Ray = Ray
  { _origin         :: !Vec3
  , _direction      :: !Vec3
  , _recipDirection :: Vec3 -- cache
  } deriving (Eq,Ord,Show,Data,Typeable,Generic)

-- | encoded to match @shaders/geometry/ray.h@
instance Block Ray

instance ToPosition Ray where
  toPosition = _origin

instance HasPosition Ray where
  position f (Ray o d r) = f o <&> \o' -> Ray o' d r

_Ray :: Iso' Ray (Vec3, Vec3) 
_Ray = iso (\(Ray o d _) -> (o, d)) $ \(o, d) -> Ray o d (recip <$> d)

class ToPosition t => ToRay t where
  toRay            :: t -> Ray

  toDirection      :: t -> Vec3
  toDirection = toDirection.toRay

  toRecipDirection :: t -> Vec3
  toRecipDirection = toRecipDirection.toRay

instance ToRay Ray where
  toRay = id
  toDirection = _direction
  toRecipDirection = _recipDirection

class (HasPosition t, ToRay t) => HasRay t where
  ray :: Lens' t Ray

  origin :: Lens' t Vec3
  origin = ray.origin

  direction :: Lens' t Vec3
  direction = ray.direction

instance HasRay Ray where
  ray = id
  origin f (Ray o d r) = f o <&> \o' -> Ray o' d r
  direction f (Ray o d r) = f d <&> \d' -> Ray o d' r

instance Field1 Ray Ray Vec3 Vec3 where
  _1 = origin

instance Field2 Ray Ray Vec3 Vec3 where
  _2 f (Ray o d _) = f d <&> \d' -> Ray o d' (recip <$> d')

class OverlapsRay t where
  -- | Check for overlap. Returns the time window along the ray when the collision occurs.
  overlapsRay :: t -> Ray -> Maybe (Float, Float)

instance OverlapsRay Box where
  overlapsRay (Box l h) (Ray o _ r) = case fold (hit <$> l <*> h <*> o <*> r) of
    (Max t0, Min t1)
      | t0 <= t1, t1 >= 0 -> Just (max 0 t0, t1)
      | otherwise -> Nothing 
    where
      hit la ha oa ra
        | a <- (la - oa) * ra
        , b <- (ha - oa) * ra
        = (Max (min a b), Min (max a b))

instance OverlapsBox Ray where
  overlapsBox r b = is _Just (overlapsRay b r)

evalRay :: Ray -> Float -> Vec3
evalRay (Ray o d _) t = o + t*^d
