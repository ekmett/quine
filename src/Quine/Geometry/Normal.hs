{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- TODO: Use cotangent and cobitangent instead
--------------------------------------------------------------------
module Quine.Geometry.Normal
  ( Normal
  , ToNormal(..)
  , HasNormal(..)
  , TangentSpace(..)
  , ToTangentSpace(..)
  , HasTangentSpace(..)
  ) where

import Control.Lens
import Data.Data
import Data.Functor
import GHC.Generics
import Linear
import Quine.GL.Types

-- * Surface normals

-- | Assumed to be a unit vector
type Normal = Vec3

class ToNormal t where
  toNormal :: t -> Normal

class ToNormal t => HasNormal t where
  normal :: Lens' t Normal

-- * Tangent spaces for a surface

-- |
--
-- We'll eventually need actual UV information in order to properly distinguish the tangent and bitangent.
--
-- For now they are merely chosen to form a space orthogonal to the surface normal.
data TangentSpace = TangentSpace
  { _tangentSpaceTangent, _tangentSpaceBitangent, _tangentSpaceNormal :: !Vec3
  } deriving (Eq,Ord,Show,Data,Typeable,Generic)

instance ToNormal TangentSpace where
  toNormal = _tangentSpaceNormal

class ToNormal t => ToTangentSpace t where
  toTangentSpace :: t -> TangentSpace
  default toTangentSpace :: ToNormal t => t -> TangentSpace
  toTangentSpace n' = TangentSpace t b n where
    n@(V3 nx ny nz) = signorm (toNormal n')
    t' | nx /= 0   = V3 ny nx nz
       | ny /= 0   = V3 nx nz ny
       | otherwise = V3 nz ny nx
    t = t' - dot n t' *^ n
    b = signorm (cross t n)

  toTangent  :: t -> Vec3
  toTangent = toTangent.toTangentSpace

  toBitangent :: t -> Vec3
  toBitangent = toBitangent.toTangentSpace

instance ToTangentSpace TangentSpace where
  toTangentSpace = id
  toTangent  = _tangentSpaceTangent
  toBitangent = _tangentSpaceBitangent

class HasNormal t => HasTangentSpace t where
  tangentSpace :: Lens' t TangentSpace
  
  tangent :: Lens' t Vec3
  tangent = tangentSpace.tangent

  bitangent :: Lens' t Vec3
  bitangent = tangentSpace.bitangent

instance HasNormal TangentSpace where
  normal f (TangentSpace t b n) = TangentSpace t b <$> f n

instance HasTangentSpace TangentSpace where
  tangentSpace = id
  tangent  f (TangentSpace t b n) = f t <&> \t' -> TangentSpace t' b n
  bitangent f (TangentSpace t b n) = f b <&> \b' -> TangentSpace t b' n

instance Field1 TangentSpace TangentSpace Vec3 Vec3 where _1 = tangent
instance Field2 TangentSpace TangentSpace Vec3 Vec3 where _2 = bitangent
instance Field3 TangentSpace TangentSpace Vec3 Vec3 where _3 = normal
