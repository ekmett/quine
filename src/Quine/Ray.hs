{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
-- Rays, these cache the recipriocal direction.
--
-- <http://tavianator.com/2011/05/fast-branchless-raybounding-box-intersections/
module Quine.Ray
  ( Ray(..)
  , _Ray
  , ToRay(..)
  , HasRay(..)
  , OverlapsRay(..)
  ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Extras
import Data.Data
import Data.Foldable
import Data.Semigroup
import GHC.Generics
import Linear
import Quine.Instances ()
import Quine.Position
import Quine.Bounding.Box

data Ray = Ray
  { _origin         :: !(V3 Double)
  , _direction      :: !(V3 Double)
  , _recipDirection :: (V3 Double) -- cache
  } deriving (Eq,Ord,Show,Data,Typeable,Generic)

instance ToPosition Ray where
  toPosition = _origin

instance HasPosition Ray where
  position f (Ray o d r) = f o <&> \o' -> Ray o' d r

_Ray :: Iso' Ray (V3 Double, V3 Double) 
_Ray = iso (\(Ray o d _) -> (o, d)) $ \(o, d) -> Ray o d (recip <$> d)

class ToPosition t => ToRay t where
  toRay            :: t -> Ray

  toDirection      :: t -> V3 Double
  toDirection = toDirection.toRay

  toRecipDirection :: t -> V3 Double
  toRecipDirection = toRecipDirection.toRay

instance ToRay Ray where
  toRay = id
  toDirection = _direction
  toRecipDirection = _recipDirection

class (HasPosition t, ToRay t) => HasRay t where
  ray :: Lens' t Ray

  origin :: Lens' t (V3 Double)
  origin = ray.origin

  direction :: Lens' t (V3 Double)
  direction = ray.direction

instance HasRay Ray where
  ray = id
  origin f (Ray o d r) = f o <&> \o' -> Ray o' d r
  direction f (Ray o d r) = f d <&> \d' -> Ray o d' r

instance Field1 Ray Ray (V3 Double) (V3 Double) where
  _1 = origin

instance Field2 Ray Ray (V3 Double) (V3 Double) where
  _2 f (Ray o d _) = f d <&> \d' -> Ray o d' (recip <$> d')

class OverlapsRay t where
  -- | Check for overlap. Returns the time window along the ray when the collision occurs.
  overlapsRay :: t -> Ray -> Maybe (Double, Double)

instance OverlapsRay Box where
  overlapsRay (Box l h) (Ray o _ r) = case fold (hit <$> l <*> h <*> o <*> r) of
    (Max t0, Min t1)
      | t0 <= t1 -> Just (t0, t1)
      | otherwise -> Nothing 
    where
      hit la ha oa ra
        | a <- (la - oa) * ra
        , b <- (ha - oa) * ra
        = (Max (min a b), Min (max a b))

instance OverlapsBox Ray where
  overlapsBox r b = is _Just (overlapsRay b r)
