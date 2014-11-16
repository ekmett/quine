{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Quine.Normal
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

-- * Surface normals

-- | Assumed to be a unit vector
type Normal = V3 Double

class ToNormal t where
  toNormal :: t -> Normal

class ToNormal t => HasNormal t where
  normal :: Lens' t Normal

-- * Tangent spaces for a surface

data TangentSpace = TangentSpace
  { _tangentSpaceTangent, _tangentSpaceBitangent, _tangentSpaceNormal :: !(V3 Double)
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

  toTangent  :: t -> V3 Double
  toTangent = toTangent.toTangentSpace

  toBitangent :: t -> V3 Double
  toBitangent = toBitangent.toTangentSpace

instance ToTangentSpace TangentSpace where
  toTangentSpace = id
  toTangent  = _tangentSpaceTangent
  toBitangent = _tangentSpaceBitangent

class HasNormal t => HasTangentSpace t where
  tangentSpace :: Lens' t TangentSpace
  
  tangent :: Lens' t (V3 Double)
  tangent = tangentSpace.tangent

  bitangent :: Lens' t (V3 Double)
  bitangent = tangentSpace.bitangent

instance HasNormal TangentSpace where
  normal f (TangentSpace t b n) = TangentSpace t b <$> f n

instance HasTangentSpace TangentSpace where
  tangentSpace = id
  tangent  f (TangentSpace t b n) = f t <&> \t' -> TangentSpace t' b n
  bitangent f (TangentSpace t b n) = f b <&> \b' -> TangentSpace t b' n

instance Field1 TangentSpace TangentSpace (V3 Double) (V3 Double) where _1 = tangent
instance Field2 TangentSpace TangentSpace (V3 Double) (V3 Double) where _2 = bitangent
instance Field3 TangentSpace TangentSpace (V3 Double) (V3 Double) where _3 = normal

