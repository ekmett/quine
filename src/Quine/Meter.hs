{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.Meter
  ( Meter(..)
  , tick
  , fps
  ) where

import Data.Data
import Data.Default
import Data.FingerTree
import Data.Monoid
import Quine.Clock

data Ticks
  = NoTicks
  | Ticks {-# UNPACK #-} !Time {-# UNPACK #-} !Time {-# UNPACK #-} !Int
  deriving (Show, Typeable, Data)

instance Monoid Ticks where
  mempty = NoTicks
  mappend NoTicks m = m
  mappend m NoTicks = m
  mappend (Ticks l _ n) (Ticks _ h n') = Ticks l h (n + n')

newtype Tick = Tick Time deriving (Show, Typeable, Data)

instance Measured Ticks Tick where
  measure (Tick d) = Ticks d d 1

newtype Meter = Meter { _meterTicks :: FingerTree Ticks Tick }
  deriving (Show, Typeable)

instance Default Meter where
  def = Meter mempty

instance Measured Ticks Meter where
  measure (Meter t) = measure t

-- | record a tick at a given time
tick :: Time -> Meter -> Meter
tick d (Meter t) = Meter $ dropUntil newEnough (t |> Tick d) where
  newEnough NoTicks       = False
  newEnough (Ticks _ h _) = h >= d - 2

-- | returns the current number of ticks per second over the last couple of seconds.
fps :: Meter -> Double
fps (Meter t) = case measure t of
  Ticks l h n | h > l -> (fromIntegral n - 1) / (h - l)
  _ -> 0 
