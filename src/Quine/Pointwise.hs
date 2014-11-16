{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Quine.Pointwise
  ( smoothstep
  , clamp
  , mix
  , step
  ) where

clamp :: Ord a => a -> a -> a -> a
clamp x l h = min (max x l) h

smoothstep :: (Fractional a, Ord a) => a -> a -> a -> a
smoothstep l h x = t*t*(3-2*t) where t = clamp ((x-l)/(h-l)) 0 1

mix :: Num a => a -> a -> a -> a
mix x y a = x*(1-a)+y*a

step :: (Num a, Ord a) => a -> a -> a 
step edge x 
  | x < edge  = 0
  | otherwise = 1

