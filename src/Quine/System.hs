{-# LANGUAGE TemplateHaskell #-}
module Quine.System
  ( System(..)
  , HasSystem(..)
  ) where

import Control.Lens
import Quine.Display
import Quine.Input
import Quine.Camera

data System = System
  { _systemDisplay :: Display
  , _systemInput   :: Input
  , _systemCamera  :: Camera
  }

makeLenses ''System

class (HasDisplay t, HasInput t, HasCamera t) => HasSystem t where
  system :: Lens' t System

instance HasDisplay System where
  display = systemDisplay

instance HasInput System where
  input = systemInput

instance HasCamera System where
  camera = systemCamera

instance HasSystem System where
  system = id
