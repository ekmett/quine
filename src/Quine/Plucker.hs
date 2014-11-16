{-# LANGUAGE TypeFamilies #-}
module Quine.Plucker
  ( t
  ) where

import Linear
import Linear.Plucker
import Quine.Ray

class ToPlucker t where
  toPlucker :: t -> Plucker Float

instance a ~ Float => ToPlucker (Plucker a) where
  toPlucker = id

instance ToPlucker Ray where
  toPlucker (Ray o d _) = Plucker ax ay az bx by bz where
    V3 ax ay az = d
    V3 bx by bz = (o + d) `cross` d
