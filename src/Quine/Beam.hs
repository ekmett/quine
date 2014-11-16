{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
-- |
module Quine.Beam
  ( Beam(..)
  , ToBeam(..)
  , HasBeam(..)
  , buildBeam
  , evalBeam
  ) where

import Control.Lens
import Data.Data
import GHC.Generics
import Linear
import Quine.Bounding.Sphere
import Quine.GL.Types
import Quine.Position
import Quine.Ray

data Beam = Beam
  { _beamRay        :: !Ray
  , _beamWidth      :: !Double
  , _beamWidthDelta :: !Double
  } deriving (Eq,Ord,Show,Data,Typeable,Generic)

instance ToPosition Beam where
  toPosition = toPosition.toRay

instance ToRay Beam where
  toRay = _beamRay

instance HasPosition Beam where
  position = ray.position

instance HasRay Beam where
  ray f (Beam r w d) = f r <&> \r' -> Beam r' w d

class ToRay t => ToBeam t where
  toBeam :: t -> Beam

  toBeamWidth :: t -> Double
  toBeamWidth = toBeamWidth.toBeam

  toBeamWidthDelta :: t -> Double
  toBeamWidthDelta = toBeamWidthDelta.toBeam

instance ToBeam Ray where
  toBeam r = Beam r 0 0

instance ToBeam Beam where
  toBeam = id
  toBeamWidth = _beamWidth
  toBeamWidthDelta = _beamWidthDelta

class HasRay t => HasBeam t where
  beam :: Lens' t Beam

  beamWidth :: Lens' t Double
  beamWidth = beam.beamWidth

  beamWidthDelta :: Lens' t Double
  beamWidthDelta = beam.beamWidthDelta
  
instance HasBeam Beam where
  beam = id
  beamWidth f (Beam r w d)      = f w <&> \w' -> Beam r w' d
  beamWidthDelta f (Beam r w d) = f d <&> \d' -> Beam r w d'

buildBeam :: (ToSphere a, ToSphere b) => a -> b -> Beam
buildBeam (toSphere -> Sphere o ow) (toSphere -> Sphere n nw) = Beam (_Ray # (o,d^*rn)) ow (dw*rn) where
  d  = n - o
  dw = nw - ow
  rn = recip $ norm d -- use a fast inverse sqrt?

evalBeam :: Beam -> Double -> (DVec3, Double)
evalBeam (Beam (Ray r d _) w dw) t = (r + t*^d, w + t*dw)
