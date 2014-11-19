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

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.State.Class hiding (get)
import Control.Lens
import Data.Default
import Linear
import Quine.GL.Types
import Quine.Input
import Quine.SDL
import Quine.StateVar

-- * Camera

data Camera = Camera
  { _fovy
  , _yaw
  , _pitch     :: !Float -- in radians
  , _cameraPos :: !Vec3
  , _cameraVel :: !Vec3
  , _nearZ     :: !Float
  , _farZ      :: !Float
  }

makeClassy ''Camera

instance Default Camera where
  def = Camera (3*pi/8) 0 0 0 0 0.1 2000
    -- fixed vertical fov = 67.5 degrees, matches old quake horizontal fov of 90.
    -- view distance from 4 inches to 2km

updateCamera :: (MonadState s m, HasCamera s, HasInput s, MonadIO m) => m ()
updateCamera = do
  V2 dx dy <- mouseRel <<.= 0
  active <- get relativeMouseMode
  when active $ do
    let ysensitivity = pi/180 -- negate for inverted mouse, TODO: take from options
        xsensitivity = pi/180
    pitch %= \x -> max (-pi/2) $ min (pi/2) (x + fromIntegral dy * ysensitivity) -- [-pi/2..pi/2]
    yaw   %= \y -> fmod (y + fromIntegral dx * xsensitivity) (2*pi)              -- [0..2*pi)

fmod :: Float -> Float -> Float
fmod a b = b * snd (properFraction $ a / b :: (Int, Float))
