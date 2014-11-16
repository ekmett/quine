{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Quine.Bounding.Box
  ( Box(..)
  , HasBox(..)
  , ToBox(..)
  , intersectBox
  ) where

import Control.Applicative
import Control.Lens
import Data.Data
import Data.Foldable
import Data.Semigroup
import GHC.Generics
import Linear
import Prelude hiding (and)
import Quine.Bounding.Sphere
import Quine.Position

data Box = Box { _lo, _hi :: V3 Double }
  deriving (Data, Typeable, Generic)

instance ToSphere Box where
  toSphere (Box l h) = Sphere (l+d) (norm d) where d = (h-l)^/2
  toRadius (Box l h) = norm $ (h-l)^/2

instance ToPosition Box where
  toPosition (Box l h) = l+(h-l)^/2

class (ToPosition t, ToSphere t) => ToBox t where
  toBox :: t -> Box

  toSize :: t -> V3 Double
  toSize = toSize.toBox

  validBox :: t -> Bool
  validBox = validBox.toBox

instance ToBox Box where
  toBox = id
  validBox (Box l h) = and $ liftA2 (<=) l h

instance ToBox Sphere where
  toBox (Sphere p r) = Box (p - r3) (p + r3) where r3 = pure r

class (HasPosition t, ToBox t) => HasBox t where
  box :: Lens' t Box

  lo :: Lens' t (V3 Double)
  lo = box.lo

  hi :: Lens' t (V3 Double)
  hi = box.hi

  size :: Lens' t (V3 Double)
  size = box.size

instance HasPosition Box where
  position f (Box l h) = f m <&> \ m' -> let d = m' - m in Box (l+d) (h+d)
    where m = l+(h-l)^/2

instance HasBox Box where
  box = id
  lo f (Box l h)   = f l <&> \l' -> Box l' h
  hi f (Box l h)   = Box l <$> f h
  size f (Box l h) = f (h-l) <&> \ s -> Box (m-s) (m+s)
    where m = l+(h-l)^/2

instance Field1 Box Box (V3 Double) (V3 Double) where
  _1 = lo

instance Field2 Box Box (V3 Double) (V3 Double) where
  _2 = hi

-- | union
instance Semigroup Box where
  Box l h <> Box l' h' = Box (liftA2 min l l') (liftA2 max h h')
  times1p _ x = x

intersectBox :: Box -> Box -> Maybe Box
intersectBox (Box l h) (Box l' h')
  | validBox result = Just result
  | otherwise       = Nothing
  where result = Box (liftA2 min l l') (liftA2 max h h')
