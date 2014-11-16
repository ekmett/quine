{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Quine.Bounding.Sphere
  ( Sphere(..)
  , HasSphere(..)
  , ToSphere(..)
  , OverlapsSphere(..)
  ) where

import Control.Lens
import Data.Data
import Data.Foldable
import Data.Functor
import GHC.Generics
import Linear
import Quine.GL.Types
import Quine.Position
import Prelude hiding (any)

data Sphere = Sphere !Vec3 !Float deriving (Eq, Ord, Show, Data, Typeable, Generic)

class ToPosition t => ToSphere t where
  toSphere :: t -> Sphere

  toRadius :: t -> Float
  toRadius t = case toSphere t of
    Sphere _ r -> r

  validSphere :: t -> Bool
  validSphere = validSphere.toSphere

instance ToPosition Sphere where
  toPosition (Sphere p _) = p

instance ToSphere Sphere where
  toSphere = id
  toRadius (Sphere _ r) = r
  validSphere (Sphere p r) = r >= 0 && not (any isNaN p)

class (HasPosition t, ToSphere t) => HasSphere t where
  sphere :: Lens' t Sphere

  radius :: Lens' t Float
  radius = sphere.radius

instance HasPosition Sphere where
  position f (Sphere p r) = f p <&> \p' -> Sphere p' r

instance HasSphere Sphere where
  sphere = id
  radius f (Sphere p r) = Sphere p <$> f r

class OverlapsSphere a where
  overlapsSphere :: a -> Sphere -> Bool

instance OverlapsSphere Sphere where
  overlapsSphere (Sphere p r) (Sphere p' r') = quadrance (p-p') < r''*r''
    where r'' = r+r'

instance a ~ Float => OverlapsSphere (V3 a) where
  overlapsSphere p (Sphere p' r) = quadrance (p-p') < r*r
