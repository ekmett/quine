{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Quine.GL.Texture
  ( TextureTarget(..)
  ) where

import Data.Data
import GHC.Generics
import Graphics.GL.Types

newtype TextureTarget = TextureTarget GLenum
  deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)
