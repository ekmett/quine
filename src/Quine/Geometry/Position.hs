{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.Geometry.Position
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
