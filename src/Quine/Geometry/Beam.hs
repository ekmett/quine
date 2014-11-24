{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.Geometry.Beam
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
import Quine.Geometry.Position
import Quine.Geometry.Ray
import Quine.Geometry.Sphere
import Quine.GL.Block
import Quine.GL.Types

-- | A 'Beam' is a fat 'Ray'. It starts out with a given 'beamWidth' and grows by 'beamWidthDelta'
-- every unit of time along the ray.
--
-- When used to model a focus the negative width is actually a real thing, so these do not stop when they hit width 0, but start to widen out again.
data Beam = Beam
  { _beamRay        :: !Ray
  , _beamWidth      :: !Float
  , _beamWidthDelta :: !Float
  } deriving (Eq,Ord,Show,Data,Typeable,Generic)

instance ToPosition Beam where
  toPosition = toPosition.toRay

instance ToRay Beam where
  toRay = _beamRay

instance HasPosition Beam where
  position = ray.position

instance HasRay Beam where
  ray f (Beam r w d) = f r <&> \r' -> Beam r' w d

-- | encoded to match @shaders/geometry/beam.h@
instance Block Beam where
  sizeOf140 _ = 44
  sizeOf430 _ = 44
  alignment140 _ = 16
  alignment430 _ = 16
  isStruct _ = True
  read140 p (Offset o) = do
    V4 a b c d <- read140 p $ Offset o
    V4 e f g h <- read140 p $ Offset (o + 16)
    i          <- read140 p $ Offset (o + 32)
    return $ Beam (Ray (V3 a b c) (V3 e f g) i) d h
  write140 p (Offset o) (Beam (Ray (V3 a b c) (V3 e f g) i) d h) = do
    write140 p (Offset o       ) (V4 a b c d)
    write140 p (Offset $ o + 16) (V4 e f g h)
    write140 p (Offset $ o + 32) i
  read430 = read140
  write430 = write140

class ToRay t => ToBeam t where
  toBeam :: t -> Beam

  toBeamWidth :: t -> Float
  toBeamWidth = toBeamWidth.toBeam

  toBeamWidthDelta :: t -> Float
  toBeamWidthDelta = toBeamWidthDelta.toBeam

instance ToBeam Ray where
  toBeam r = Beam r 0 0

instance ToBeam Beam where
  toBeam = id
  toBeamWidth = _beamWidth
  toBeamWidthDelta = _beamWidthDelta

class HasRay t => HasBeam t where
  beam :: Lens' t Beam

  beamWidth :: Lens' t Float
  beamWidth = beam.beamWidth

  beamWidthDelta :: Lens' t Float
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

evalBeam :: Beam -> Float -> (Vec3, Float)
evalBeam (Beam (Ray r d _) w dw) t = (r + t*^d, w + t*dw)
