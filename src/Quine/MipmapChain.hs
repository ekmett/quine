{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- A chain of textures with decreasing sizes (currently not enforced)
--------------------------------------------------------------------
module Quine.MipmapChain
  ( -- * Chained Mipmap Images
    MipmapChain
  -- ** Construction
  , mkMipmapChain
  , mipMapChain
  -- ** Query
  , mipMapBase
  , maxMipMapLevel
  -- ** Predicates
  , hasMipMaps
  ) where

import Prelude hiding (zipWith,tail,head,length,sequence_)
import Data.Foldable (sequence_)
import Data.List.NonEmpty
import Quine.Image

-- | The first element in the 'MipmapChain' is the base element
-- (e.g. 'Texture' or 'FilePath'). Every 'MipmapChain' has a base
-- element (aka is never empty see: 'NonEmpty'). The other elements in the chain are the mipmap
-- levels in resoloution descending order.
type MipmapChain tex = NonEmpty tex

-- | Creates a mipmap chain from a base and a mip map list
mkMipmapChain :: tex -> [tex] -> MipmapChain tex
mkMipmapChain base mipmaps = base :| mipmaps
{-# INLINE mkMipmapChain #-}

-- | creates a 'MipmapChain' from a list. At least one element as the base element
-- is required. On an empty list 'Nothing' is returned.
mipMapChain :: [tex] -> Maybe (MipmapChain tex)
mipMapChain = nonEmpty
{-# INLINE mipMapChain #-}

-- | Extracts the first element in the mipmap chain
mipMapBase :: MipmapChain tex -> tex
mipMapBase = head
{-# INLINE mipMapBase #-}

-- | Predicate if a `MipmapChain` has destinct mipmap levels beside the base (0th)
hasMipMaps :: MipmapChain a -> Bool
hasMipMaps = not . null . tail
{-# INLINE hasMipMaps #-}

-- | The number of mipmap levels (without the base)
maxMipMapLevel :: MipmapChain tex -> Int
maxMipMapLevel mips = length mips - 1
{-# INLINE maxMipMapLevel #-}

instance Image2D a => Image2D (MipmapChain a) where
  upload chain t _l = sequence_ $ zipWith (\img l -> upload img t l) chain (mkMipmapChain 0 [1..])
