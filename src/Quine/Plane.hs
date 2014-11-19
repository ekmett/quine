{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Quine.Plane
  ( Plane(..)
  , ToPlane(..)
  , HasPlane(..)
  , signedDistance
  ) where

import Control.Applicative
import Control.Lens
import Data.Data
import GHC.Generics
import Linear
import Quine.Normal
import Quine.GL.Types

-- serialize as a 4d vector
newtype Plane = Plane Vec4 deriving (Show,Ord,Eq,Typeable,Data,Generic)

instance ToNormal Plane where
  toNormal (Plane (V4 a b c _)) = V3 a b c

instance HasNormal Plane where
  normal f (Plane n) = Plane <$> _xyz f n

class ToNormal a => ToPlane a where
  toPlane :: a -> Plane

instance ToPlane Plane where
  toPlane = id

class (HasNormal a, ToPlane a) => HasPlane a where
  plane    :: Lens' a Plane
  distance :: Lens' a Float

instance HasPlane Plane where
  plane = id
  distance f (Plane (V4 a b c d)) = Plane . V4 a b c <$> f d

signedDistance :: Plane -> Vec3 -> Float
signedDistance (Plane (V4 a b c d)) (V3 x y z) = a*x + b*y + c*z + d
