{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.Camera 
  ( Camera(..)
  , HasCamera(..)
  , updateCamera
  ) where

import Control.Monad.State.Class
import Control.Lens
import Data.Default
import Linear
import Quine.GL.Types
import Quine.Input

-- * Camera

data Camera = Camera
  { _fov, _yaw, _pitch :: Float -- in radians
  , _cameraPos :: Vec3
  , _cameraVel :: Vec3
  }

makeClassy ''Camera

instance Default Camera where
  def = Camera (pi/2) 0 0 0 0

updateCamera :: (MonadState s m, HasCamera s, HasInput s) => m ()
updateCamera = do
  let ysensitivity = pi/180 -- negate for inverted mouse, TODO: take from options
      xsensitivity = pi/180
  V2 dx dy <- mouseRel <<.= 0
  pitch %= \x -> max (-pi/2) $ min (pi/2) (x + fromIntegral dy * ysensitivity) -- [-pi/2..pi/2]
  yaw   %= \y -> fmod (y + fromIntegral dx * xsensitivity) (2*pi)              -- [0..2*pi)

fmod :: Float -> Float -> Float
fmod a b = b * snd (properFraction $ a / b :: (Int, Float))
