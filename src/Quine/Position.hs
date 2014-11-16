{-# LANGUAGE DefaultSignatures #-}
module Quine.Position
  ( ToPosition(..)
  , HasPosition(..)
  ) where

import Control.Lens
import Linear

class ToPosition t where
  toPosition :: t -> V3 Double
  default toPosition :: HasPosition t => t -> V3 Double
  toPosition = view position

class ToPosition t => HasPosition t where
  position :: Lens' t (V3 Double)
