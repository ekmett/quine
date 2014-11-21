{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.Geometry.Box
  ( Box(..)
  , HasBox(..)
  , ToBox(..)
  , intersectBox
  , OverlapsBox(..)
  ) where

import Control.Applicative
import Control.Lens
import Data.Data
import Data.Foldable
import Data.Semigroup
import GHC.Generics
import Linear
import Prelude hiding (and)
import Quine.Geometry.Position
import Quine.Geometry.Sphere
import Quine.GL.Types

data Box = Box { _lo, _hi :: Vec3 }
  deriving (Data, Typeable, Generic)

instance ToSphere Box where
  toSphere (Box l h) = Sphere (l+d) (norm d) where d = (h-l)^/2
  toRadius (Box l h) = norm $ (h-l)^/2

instance ToPosition Box where
  toPosition (Box l h) = l+(h-l)^/2

class (ToPosition t, ToSphere t) => ToBox t where
  toBox :: t -> Box

  toSize :: t -> Vec3
  toSize = toSize.toBox

  validBox :: t -> Bool
  validBox = validBox.toBox

  corners :: t -> [Vec3]
  corners = corners.toBox

instance ToBox Box where
  toBox = id
  validBox (Box l h) = and $ liftA2 (<=) l h
  corners (Box (V3 lx ly lz) (V3 hx hy hz)) =
    V3 <$> [lx,hx] <*> [ly,hy] <*> [lz,hz]

instance ToBox Sphere where
  toBox (Sphere p r) = Box (p - r3) (p + r3) where r3 = pure r

class (HasPosition t, ToBox t) => HasBox t where
  box :: Lens' t Box

  lo :: Lens' t Vec3
  lo = box.lo

  hi :: Lens' t Vec3
  hi = box.hi

  size :: Lens' t Vec3
  size = box.size

instance HasPosition Box where
  position f (Box l h) = f m <&> \ m' -> let d = m' - m in Box (l+d) (h+d)
    where m = l+(h-l)^*0.5

instance HasBox Box where
  box = id
  lo f (Box l h)   = f l <&> \l' -> Box l' h
  hi f (Box l h)   = Box l <$> f h
  size f (Box l h) = f (h-l) <&> \ s -> Box (m-s) (m+s)
    where m = l+(h-l)^*0.5

instance Field1 Box Box Vec3 Vec3 where
  _1 = lo

instance Field2 Box Box Vec3 Vec3 where
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

instance OverlapsSphere Box where
  -- TODO: consider <http://www.mrtc.mdh.se/projects/3Dgraphics/paperF.pdf>?
  overlapsSphere (Box l h) (Sphere p r) = quadrance (liftA3 arvo l h p) < r*r where
    arvo a b c
      | c < a = c - a
      | c > b = c - b
      | otherwise = 0

class OverlapsBox a where
  overlapsBox :: a -> Box -> Bool

instance OverlapsBox Box where
  overlapsBox (Box al ah) (Box bl bh)
     = and (liftA2 (<=) al bh)
    && and (liftA2 (<=) bl ah)

instance OverlapsBox Sphere where
  overlapsBox = flip overlapsSphere

instance a ~ Float => OverlapsBox (V3 a) where
  overlapsBox (V3 a b c) (Box (V3 la lb lc) (V3 ha hb hc)) =
    la <= a && a <= ha && lb <= b && b <= hb && lc <= c && c <= hc
