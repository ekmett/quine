{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Quine.Bounding.Sphere
  ( Sphere(..)
  , HasSphere(..)
  , ToSphere(..)
  ) where

import Control.Lens
import Data.Data
import Data.Foldable
import Data.Functor
import GHC.Generics
import Linear
import Quine.Position
import Prelude hiding (any)

data Sphere = Sphere !(V3 Double) !Double deriving (Eq, Ord, Show, Data, Typeable, Generic)

class ToPosition t => ToSphere t where
  toSphere :: t -> Sphere

  toRadius :: t -> Double
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

  radius :: Lens' t Double
  radius = sphere.radius

instance HasPosition Sphere where
  position f (Sphere p r) = f p <&> \p' -> Sphere p' r

instance HasSphere Sphere where
  sphere = id
  radius f (Sphere p r) = Sphere p <$> f r
