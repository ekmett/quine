{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

type Normal = V3 Double

class ToNormal t where
  toNormal :: t -> Normal

class ToNormal t => HasNormal t where
  normal :: Lens' t Normal

data TangentSpace = TangentSpace
  { _tangentSpaceTangent, _tangentSpaceBinormal, _tangentSpaceNormal :: !(V3 Double)
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

  toBinormal :: t -> V3 Double
  toBinormal = toBinormal.toTangentSpace
  toTangent  :: t -> V3 Double
  toTangent = toTangent.toTangentSpace

instance ToTangentSpace TangentSpace where
  toTangentSpace = id
  toBinormal = _tangentSpaceBinormal
  toTangent  = _tangentSpaceTangent

class HasNormal t => HasTangentSpace t where
  tangentSpace :: Lens' t TangentSpace
  
  binormal :: Lens' t (V3 Double)
  binormal = tangentSpace.binormal

  tangent :: Lens' t (V3 Double)
  tangent = tangentSpace.tangent

instance HasNormal TangentSpace where
  normal f (TangentSpace t b n) = TangentSpace t b <$> f n

instance HasTangentSpace TangentSpace where
  tangentSpace = id
  binormal f (TangentSpace t b n) = f b <&> \b' -> TangentSpace t b' n
  tangent  f (TangentSpace t b n) = f t <&> \t' -> TangentSpace t' b n
