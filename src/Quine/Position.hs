{-# LANGUAGE DefaultSignatures #-}
module Quine.Position
  ( Position
  , ToPosition(..)
  , HasPosition(..)
  ) where

import Control.Lens
import Quine.GL.Types

type Position = Vec3

class ToPosition t where
  toPosition :: t -> Vec3
  default toPosition :: HasPosition t => t -> Vec3
  toPosition = view position

class ToPosition t => HasPosition t where
  position :: Lens' t Vec3
