{-# OPTIONS -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
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
