module Engine.Transparency where

import Control.Applicative
import Data.Monoid
import Linear

-- Weighted-Blended Order Independent Transparency by Morgan McGuire and Louis Bavoil
--
-- Accumulators likely need FP16 precision, color overflows
--
-- This assumes premultiplied alpha.

data OIT f a = OIT !a !a !(f a)
  deriving (Eq,Ord,Show,Read)

-- The OIT scheme of McGuire and Bavoil naturally extends to a monoidal accumulator
instance (Additive f, Num a) => Monoid (OIT f a) where
  mempty = OIT 0 1 zero
  mappend (OIT wa1 na1 c1) (OIT wa2 na2 c2) = OIT (wa1 + wa2) (na1 * na2) (c1 ^+^ c2)
  
-- build an order independent overlay
weighted :: (Functor f, Num a) => (a -> a -> a) -> a -> f a -> a -> OIT f a
weighted w z ci ai = OIT (m * ai) (1 - ai) (m *^ ci)
  where m = w z ai

-- blend an overlay into the background
blend :: (Additive f, Fractional a) => OIT f a -> f a -> f a
blend (OIT was nas cs) c0 = (nas *^ c0) ^+^ ((1 - nas) / was) *^ cs
