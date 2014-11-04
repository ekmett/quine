{-# LANGUAGE TypeSynonymInstances #-}
module Quine.GL.Classes
  ( GL(..)
  , Object(..)
  ) where

import Control.Lens
import Graphics.GL.Raw.Types

class GL t where
  _GL :: Prism' GLenum t

instance GL GLenum where
  _GL = id

class Object t where
  object :: t -> GLuint

instance Object GLuint where
  object = id


