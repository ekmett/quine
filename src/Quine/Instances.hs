{-# OPTIONS -fno-warn-orphans #-}
module Quine.Instances () where

import Numeric.Half

instance Bounded Double where
  maxBound = 1/0
  minBound = -1/0

instance Bounded Float where
  maxBound = 1/0
  minBound = -1/0

instance Bounded Half where
  maxBound = 1/0
  minBound = -1/0
